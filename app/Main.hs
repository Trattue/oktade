module Main where

import Data.Attoparsec.ByteString.Lazy (Result (Done, Fail))
import Data.ByteString.Lazy as BS (isPrefixOf, readFile, stripSuffix)
import Data.Oktade.Classfile (encodeClassfile, parseClassfile)

main :: IO ()
main = do
  classfile <- BS.readFile "test.class"
  case parseClassfile classfile of
    Fail i c e -> do
      putStr "Parsing failed! Error: "
      print e
      putStrLn "Remaining input:"
      print i
    Done i r -> do
      print r
      putStr "\nTesting homomorphism... "
      let encoded = encodeClassfile r
      if encoded `isPrefixOf` classfile
        then putStrLn "success!"
        else putStrLn "failed!"
  -- putStrLn $
  --   "Generated output:\n" ++ show encoded ++ "\nInput:\n" ++ show classfile
  return ()
