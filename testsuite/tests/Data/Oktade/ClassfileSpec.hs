module Data.Oktade.ClassfileSpec (spec) where

import Control.Monad (forM)
import Data.Attoparsec.ByteString.Lazy (Result (Done, Fail))
import qualified Data.ByteString.Lazy as BS (isPrefixOf, readFile)
import Data.List (isSuffixOf)
import Data.Oktade.Classfile (parseClassfile, unparseClassfile)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "oktade" $ do
    it "parses JVM classfiles" $ do
      paths <- cfrTestsPaths
      mapM_ (\p -> do parse p `shouldReturn` True) paths
    it "homomorphism parsing and unparsing" $ do
      paths <- cfrTestsPaths
      mapM_ (\p -> do content p `shouldReturn` True) paths
  where
    parse p = do
      classfile <- BS.readFile p
      case parseClassfile classfile of
        Fail {} -> error $ show p
        Done {} -> return True
    content p = do
      classfile <- BS.readFile p
      case parseClassfile classfile of
        Fail {} -> error $ show p
        (Done _ result) -> do
          return $ unparseClassfile result `BS.isPrefixOf` classfile

cfrTestsPaths :: IO [FilePath]
cfrTestsPaths = do
  currentDir <- getCurrentDirectory
  let testDir = currentDir </> "testsuite" </> "resources" </> "cfr-tests"
  walk [] testDir
  where
    walk :: [FilePath] -> FilePath -> IO [FilePath]
    walk fs p = do
      isFile <- doesFileExist p
      if isFile
        then if ".class" `isSuffixOf` p then return $ p : fs else return fs
        else walkDir fs p
    walkDir :: [FilePath] -> FilePath -> IO [FilePath]
    walkDir fs p =
      do
        content <- listDirectory p
        concat <$> forM ((p </>) <$> content) (walk fs)
