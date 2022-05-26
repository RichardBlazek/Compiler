module Main where

import qualified System.IO as IO
import qualified System.Environment as Env

import Loader (load)
import Codegen (gen)
import Fallible (unwrap)

compile :: [String] -> IO ()
compile (input : output : _) = unwrap (IO.hPutStrLn IO.stderr) (IO.writeFile output) (fmap (uncurry gen) $ load input) >>= id
compile _ = IO.hPutStrLn IO.stderr "Input or output file missing"

main :: IO ()
main = Env.getArgs >>= compile
