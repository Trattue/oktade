module Main where

import Data.Attoparsec.ByteString.Lazy (Result (Done, Fail))
import qualified Data.ByteString.Lazy as BS (readFile)
import Data.Oktade.Classfile (parseClassfile)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    (path : _) -> do
      classfile <- BS.readFile path
      case parseClassfile classfile of
        Fail _ _ e ->
          putStrLn $
            "Failed parsing file " ++ show path ++ ", is it a valid classfile?"
        Done _ r -> print r
    _ -> abort
  where
    abort =
      let usage = "Usage: ./oktade <filepath>\n<filepath>: Path to a classfile."
       in do
            die usage
