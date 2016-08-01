module Main where

import System.Console.CmdArgs

import Application.MiscUtils.ProgType
import Application.MiscUtils.Command

main :: IO () 
main = do 
  putStrLn "misc-utils"
  param <- cmdArgs mode

  commandLineProcess param