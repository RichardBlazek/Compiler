module Loader (load) where

import qualified Data.Map as Map
import qualified Data.Tree as Tree
import qualified Data.List as List
import qualified Network.HTTP.Conduit as Http
import qualified Data.ByteString.Lazy.Char8 as Char
import qualified System.FilePath as Path
import qualified Scope
import qualified Parser
import qualified Semantics
import qualified Language as Lang
import Fallible (Fallible, FallibleIO, err, correct, wrap)
import Data.Maybe (catMaybes)
import Data.Containers.ListUtils (nubOrdOn)
import Functions (mapCatFoldlM, map2, fmap2, leaf, listify)

data File = Zyba Parser.File | Php String
data Lang = ZybaLang | PhpLang deriving (Eq, Show)
data LoadedFile = LoadedFile File [String] [String]
type Cache = Map.Map String LoadedFile
type Dependencies = Map.Map String (Tree.Tree (String, File))

isWeb :: String -> Bool
isWeb path = List.isPrefixOf "http://" path || List.isPrefixOf "https://" path

readPath :: String -> IO String
readPath p
  | null path = return ""
  | isWeb path = fmap Char.unpack $ Http.simpleHttp path
  | otherwise = readFile path
  where path = dropWhile (`elem` " \t\n\r\f\v") p

combinePath :: String -> String -> String
combinePath file imported
  | isWeb imported = imported
  | otherwise = Path.combine (Path.takeDirectory file) imported

loadFile :: Lang -> String -> FallibleIO LoadedFile
loadFile PhpLang path = correct (readPath path) >>= \content -> return (LoadedFile (Php content) [] [])
loadFile ZybaLang path = do
  content <- correct $ readPath path
  parsed@(Parser.File declarations) <- wrap $ Parser.parse (combinePath path) content
  return $ uncurry (LoadedFile $ Zyba parsed) $ importsFrom declarations
  where import' (_, _, Parser.Import _ imported) = (Just imported, Nothing)
        import' (_, _, Parser.Php _ imported _) = (Nothing, Just imported)
        import' _ = (Nothing, Nothing)
        importsFrom = map2 catMaybes catMaybes . unzip . map import'

lookupCache :: Lang -> Cache -> String -> FallibleIO (Cache, LoadedFile)
lookupCache lang cache path = case Map.lookup path cache of
  Just file@(LoadedFile (Zyba _) _ _) | lang == ZybaLang -> return (cache, file)
  Just file@(LoadedFile (Php _) _ _) | lang == PhpLang -> return (cache, file)
  _ -> do
    file <- loadFile lang path
    return (Map.insert path file cache, file)

loadDependencies :: Cache -> [String] -> Dependencies -> FallibleIO Dependencies
loadDependencies _ [] deps = return deps
loadDependencies cache (path : upper) deps = do
  (cache, LoadedFile file zybas phps) <- lookupCache ZybaLang cache path
  (cache, phpContents) <- getPhps cache phps
  let children = map (deps Map.!) zybas ++ zipWith (curry leaf) phps phpContents
  case filter (`Map.notMember` deps) zybas of
    [] -> loadDependencies cache upper $ Map.insert path (Tree.Node (path, file) children) deps
    next : _ -> checkCircular next >> loadDependencies cache (next : path : upper) deps
  where getPhps = mapCatFoldlM (\cache' path' -> fmap2 id (listify . getFile) $ lookupCache PhpLang cache' path')
        getFile (LoadedFile file _ _) = file
        checkCircular path = if path `elem` upper then wrap $ err (-1) $ "Circular dependency: " ++ path else return ()

orderDependencies :: Ord a => Tree.Tree (a, b) -> [(a, b)]
orderDependencies = nubOrdOn fst . concat . reverse . Tree.levels

listOfImportedFiles :: String -> FallibleIO [(String, File)]
listOfImportedFiles path = do
  deps <- loadDependencies Map.empty [path] Map.empty 
  return $ orderDependencies $ deps Map.! path

analyseAll :: Map.Map String Scope.Scope -> [String] -> [(String, [Semantics.Declaration])] -> [(String, File)] -> Fallible ([String], [(String, [Semantics.Declaration])])
analyseAll _ phps zybas [] = return $ (reverse phps, reverse zybas)
analyseAll known phps zybas ((_, Php content) : paths) = analyseAll known (content : phps) zybas paths
analyseAll known phps zybas ((path, Zyba parsed) : paths) = do
  (scope, declarations) <- Semantics.analyse known path parsed
  analyseAll (Map.insert path scope known) phps ((path, declarations) : zybas) paths

load :: String -> FallibleIO ([String], [(String, [Semantics.Declaration])])
load path = listOfImportedFiles path >>= wrap . analyseAll Map.empty [] []
