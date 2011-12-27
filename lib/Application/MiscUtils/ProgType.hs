{-# LANGUAGE DeriveDataTypeable #-}

module Application.MiscUtils.ProgType where 

import System.Console.CmdArgs

data Misc_utils = Test 
                | FindXoj { machine :: String 
                          , dest :: FilePath }
                | ConvertXoj  
              deriving (Show,Data,Typeable)

test :: Misc_utils
test = Test 

findxoj :: Misc_utils 
findxoj = FindXoj { machine = "" &= typ "MACHINE" &= argPos 0
                  , dest = "" &= typ "DESTINATION" &= argPos 1 
                  }  

convertxoj :: Misc_utils
convertxoj = ConvertXoj 

mode :: Misc_utils
mode = modes [test, findxoj, convertxoj]


