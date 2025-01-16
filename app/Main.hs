module Main where

import Ir
import Parser
import CodeGen

import System.IO
import System.Environment
import System.Exit (die)
import Control.Exception (try)
import Data.Either (either)

filterBf :: String -> String
filterBf = filter (`elem` "><+-.,[]")

readBfFile :: FilePath -> IO (Either String String)
readBfFile path = do
  handle <- try (readFile path) :: IO (Either IOError String)
  return $ case handle of
    Left err -> Left $ "Error reading file: " ++ show err
    Right content -> Right $ filterBf content


main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      program <- readBfFile path
      case program of
        Left err -> die err
        Right code -> do
          let instructions = progToIr (parseProgram code)
          writeFile "output.c" (generateC instructions)
    _ -> die "Usage: hsbf <path-to-bf-file>"
