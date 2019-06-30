module Main where

import qualified Data.Text as T

import Data.Resheet
import Data.Resheet.Parser

main :: IO ()
main = interact $ \s -> case parseSong "<stdin>" s of
                          Left err -> error (show err)
                          Right v -> T.unpack (outputSong v)
