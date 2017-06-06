module Main where

import Ed (ed)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= ed
