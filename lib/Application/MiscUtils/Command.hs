module Application.MiscUtils.Command where

import Application.MiscUtils.ProgType
import Application.MiscUtils.Job

commandLineProcess :: Misc_utils -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
