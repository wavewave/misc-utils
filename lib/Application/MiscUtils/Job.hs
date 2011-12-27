module Application.MiscUtils.Job where

import Control.Monad
import System.Posix.Files 
import System.Directory 
import System.FilePath 
import System.IO 


startJob :: IO () 
startJob = do 
  putStrLn "job started"


startFindXoj :: IO ()
startFindXoj = do 
  putStrLn "find all xoj files" 
  cdir <- getCurrentDirectory 
  ls <- getDirectoryContents cdir 
  mapM_ (\x -> do { putStrLn $ x ++ ":"  ; action x } ) ls 

subdiraction :: FilePath -> (FilePath -> IO ()) -> IO () 
subdiraction path action = do 
  getFileStatus path >>= \status -> 
    when (isDirectory status 
          && last path /= '.'
          && head path /= '_'
          && head path /= '.'
         ) (action path)

action :: FilePath -> IO () 
action path = subdiraction path $ \x -> do 
                ls <- getDirectoryContents x 
                mapM_ (\x -> do { putStrLn x; action x }  ) 
                  . map (path </>) 
                  $ ls  



