module Application.MiscUtils.Command where

import Application.MiscUtils.ProgType
import Application.MiscUtils.Job

commandLineProcess :: Misc_utils -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
commandLineProcess (FindXoj machine dest) = do 
  putStrLn "findxoj called"
  startFindXoj machine dest
commandLineProcess ConvertXoj = do 
  putStrLn "convertxoj called"
  startConvertXoj 

