module Application.MiscUtils.Job where

import Control.Monad
import Data.Maybe 

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
  findxoj cdir 

subdiraction :: FilePath -> (FilePath -> IO ()) -> IO () 
subdiraction path action = do
  -- putStrLn $ " path = " ++ path 
  checkDirExistAndDo path $ \item -> do 
    -- putStrLn $ "exist : " ++ item 
    getFileStatus item >>= \status -> 
      when (isDirectory status 
            && not (isSymbolicLink status)
            && not (isDotPath item) 
           ) (action item)


isDotPath :: FilePath -> Bool 
isDotPath fp = 
  let paths = splitPath fp
      dirs = filter (\x -> last x == '/') paths 
      filename = takeFileName fp  
      basename = takeBaseName fp
      dotpathtest = do 
        mapM_ (\x -> trueNothing (head x== '.')) dirs 
        trueNothing (filename == "." || filename == "..")
        bname <- safeNonNull basename
        trueNothing (head bname == '.' || head bname == '_')
  in not . isJust $ dotpathtest 

safeNonNull :: [a] -> Maybe [a]
safeNonNull [] = Nothing 
safeNonNull (x:xs) = Just (x:xs)

trueNothing :: Bool -> Maybe () 
trueNothing True = Nothing 
trueNothing False = Just ()

findxoj :: FilePath -> IO () 
findxoj path = do  
  shortpaths <- getDirectoryContents path 
  let fullpaths = map (path </>) shortpaths
  forM_ fullpaths $ \item -> do 
    checkFileExistAndDo item $ \fp -> do 
      status <- getFileStatus fp 
      when (isRegularFile status && not (isSymbolicLink status)) $
        checkXoj fp 
  forM_ fullpaths $ \item -> do 
    subdiraction item findxoj 

checkFileExistAndDo :: FilePath -> (FilePath -> IO ()) -> IO ()
checkFileExistAndDo fp action = doesFileExist fp >>= \b -> when b (action fp)

checkDirExistAndDo :: FilePath -> (FilePath -> IO ()) -> IO ()
checkDirExistAndDo fp action = doesDirectoryExist fp >>= \b -> when b (action fp)


checkXoj :: FilePath -> IO () 
checkXoj fp = do 
  let (fname_wo_ext,fname_ext) = splitExtension fp
  when (fname_ext == ".xoj") $ 
    putStrLn $ " bingo = " ++ fp


