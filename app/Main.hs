module Main where

import Lib
import Parse
import Data.Char (toUpper)
import System.Directory (renameFile, getTemporaryDirectory)
import System.Environment (getArgs)
import Control.Monad (when)

-- main :: IO ()
-- main = do
--   [file] <- getArgs
--   tmpDir <- getTemporaryDirectory
--   let tmpFile = tmpDir ++ "/" ++ file
--   readFile file >>= writeFile tmpFile . map toUpper
--   renameFile tmpFile file
-- main = do
--   contents <- readFile "file.txt"
--   let newContents = map toUpper contents
--   when (length newContents > 0) $
--     writeFile "file.txt" newContents

main = main7
