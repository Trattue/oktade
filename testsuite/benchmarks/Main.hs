module Main
  ( main,
  )
where

import Control.Applicative (liftA)
import Control.Monad (forM, forM_, replicateM_, unless)
import Data.Attoparsec.ByteString.Lazy (Result (Done, Fail))
import qualified Data.ByteString.Lazy as BS (readFile)
import Data.List (isSuffixOf)
import Data.Oktade.Classfile (parseClassfile)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Internals (puts)

main :: IO ()
main = do
  putStrLn ""
  putStrLn $
    "One iteration of this benchmark parses the whole cfr-tests project"
      ++ " sequentially."
  putStrLn "Only file reading and parsing will be measured.\n"
  paths <- cfrTestsPaths
  let warmupCount = 10
  putStrLn $ "Warmup, running " ++ show warmupCount ++ " iterations..."
  replicateM_ warmupCount $ parseClassfiles paths
  let repeatCount = 200
  putStrLn $ "Benchmark, running " ++ show repeatCount ++ " iterations..."
  start <- getTime Monotonic
  replicateM_ repeatCount $ parseClassfiles paths
  end <- getTime Monotonic
  let nsDuration = toNanoSecs (diffTimeSpec start end)
  let msDuration = nsDuration `div` 1000000
  let avgMsDuration = nsDuration `div` fromIntegral repeatCount `div` 1000000
  putStrLn $
    "Benchmark took "
      ++ show msDuration
      ++ "ms ("
      ++ show avgMsDuration
      ++ "ms per iteration in average).\n"

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

parseClassfiles :: [FilePath] -> IO ()
parseClassfiles = mapM_ parse
  where
    parse p = do
      classfile <- BS.readFile p
      case parseClassfile classfile of
        Fail i c e -> putStrLn $ "Warning: Failed parsing " ++ show p
        Done i r -> return ()
