{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.JSON
import Text.JSON.Git.Pretty

main = do
  (res :: Result JSValue) <- readFile file >>= return . decode
  --let res = decode c
  case res of
    Ok obj -> writeFile file2 $ show $ pp_value obj
    Error err -> putStrLn err
  where
    file = "sample.json"
    file2 = "sample2.json"
