module Main where

import System.Console.Haskeline

import Data.Attoparsec.Text
import Data.Text
import Data.Word

import Parser

main = do
  putStrLn "Hello, World!"
  runInputT defaultSettings repl

run line = do
  let expr = eitherResult $ parse lettuce $ pack line
  case expr of
    Left s -> s
    Right r -> show (lbind r :: LettuceBinding Text Word)

repl = do
  maybeLine <- getInputLine "% "
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      outputStrLn (run line)
      repl
