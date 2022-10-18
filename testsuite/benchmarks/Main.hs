module Main
  ( main,
  )
where

import Control.Monad (forM, replicateM_)
import Data.Attoparsec.ByteString.Lazy (Result (Done, Fail))
import Data.ByteString.Lazy (isPrefixOf)
import qualified Data.ByteString.Lazy as BS (readFile)
import Data.List (isSuffixOf)
import Data.Oktade.Classfile (parseClassfile, unparseClassfile)
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  intro
  paths <- cfrTestsPaths
  warmup 10 paths
  benchmark 200 paths

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

intro :: IO ()
intro = do
  putStrLn $
    "\nOne iteration of this benchmark parses and unparses all classfiles in the"
      ++ " cfr-tests project sequentially."
  putStrLn
    "Only file reading and parsing will be measured.\n"

warmup :: Int -> [FilePath] -> IO ()
warmup c ps = do
  putStrLn $ "Warmup, running " ++ show c ++ " iterations..."
  run c ps

benchmark :: Int -> [FilePath] -> IO ()
benchmark c ps = do
  putStrLn $ "Benchmark, running " ++ show c ++ " iterations..."
  nsDuration <- timeNanoSecs $ run c ps
  let msDuration = nsDuration `div` 1000000
  let avgMsDuration = nsDuration `div` fromIntegral c `div` 1000000
  evaluation msDuration avgMsDuration

timeNanoSecs :: IO () -> IO Integer
timeNanoSecs a = do
  start <- getTime Monotonic
  a
  end <- getTime Monotonic
  return $ toNanoSecs (diffTimeSpec start end)

run :: Int -> [FilePath] -> IO ()
run c ps = replicateM_ c $ parseClassfiles ps

parseClassfiles :: [FilePath] -> IO ()
parseClassfiles = mapM_ parse
  where
    parse p = do
      classfile <- BS.readFile p
      case parseClassfile classfile of
        Fail {} -> putStrLn $ "Warning: Failed parsing " ++ show p
        (Done _ result) -> do
          if unparseClassfile result `isPrefixOf` classfile
            then return ()
            else putStrLn $ "Warning: Failed homomorphism " ++ show p

evaluation :: Integer -> Integer -> IO ()
evaluation ms avgMs =
  putStrLn $
    "Benchmark took "
      ++ show ms
      ++ "ms ("
      ++ show avgMs
      ++ "ms per iteration on average).\n"
