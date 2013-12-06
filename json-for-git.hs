{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad (filterM)
import           System.Environment (getArgs)
import           System.Exit
import           System.FilePath (takeExtension)
import           Text.JSON
import           Text.JSON.Git.Pretty
import           Util.File (recurseDirs)
import qualified Data.ByteString as B

main :: IO ()
main = do
  files <- getArgs
  case files of
    [] -> do
      putStrLn "NAME"
      putStrLn "    jsonforgit"
      putStrLn "\nSYNOPSIS"
      putStrLn "    jsonforgit [-R] [space delimited list of files to be prettified]"
      putStrLn "\nOPTIONS"
      putStrLn "    -R prettify recursively all json files starting"
      putStrLn "       with the currenty working directory"
    (option:_) -> case option of
      "-R" -> allJSON >>= mapM_ readWrite
      otherwise -> mapM_ readWrite files
  return ()

readWrite :: FilePath -> IO ()
readWrite fp = do
  json <- readFile fp
  let (result :: Result JSValue) = decode json
  case result of
    Ok a -> writeFile fp $ render $ pp_value a
    Error err -> do
      putStrLn err
      exitFailure

allJSON :: IO [FilePath]
allJSON = recurseDirs "." >>= filterM isJSON >>= mapM (return . drop 2)
  where
    isJSON :: FilePath -> IO Bool
    isJSON = return . (==) ".json" . takeExtension
