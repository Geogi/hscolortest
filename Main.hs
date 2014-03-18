module Main where

import Control.Monad

import System.Console.Terminfo.Base
import System.Console.Terminfo.Color

import Format

someText :: Formatted
someText = format "some text\n" Blue

moreText :: Formatted
moreText = compose (format "more text\n" Red) Bold

main :: IO ()
main = do
  fprint someText
  fprint moreText
