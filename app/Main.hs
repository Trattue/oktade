module Main where

import Control.Monad (forM_, unless)
import Data.Attoparsec.ByteString.Lazy (Result (Done, Fail))
import Data.ByteString.Lazy as BS (isPrefixOf, readFile, stripSuffix)
import Data.List (isSuffixOf)
import Data.Oktade.Classfile (encodeClassfile, parseClassfile)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath ((</>))

main :: IO ()
main = test

test :: IO ()
test = do
  dir <- getCurrentDirectory
  let testDir = dir </> "tests"
  walk testDir
  where
    walk path = do
      content <- listDirectory path
      forM_ ((path </>) <$> content) walk'
    walk' p = do
      isFile <- doesFileExist p
      if isFile && ".class" `isSuffixOf` p
        then
          ( do
              classfile <- BS.readFile p
              case parseClassfile classfile of
                Fail i c e -> putStrLn $ show p ++ "... Failed!"
                Done i r -> do
                  -- print r
                  putStrLn $ show p ++ "... Success!"
          )
        else unless isFile $ do walk p
