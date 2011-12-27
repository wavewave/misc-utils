module Application.MiscUtils.Job where

import Control.Monad

import Data.List 
import Data.Maybe 

import System.Posix.Files 
import System.Directory 
import System.FilePath 
import System.Process
import System.IO 


startJob :: IO () 
startJob = do 
  putStrLn "job started"


startFindXoj :: String -> FilePath -> IO ()
startFindXoj machine dest = do 
  putStrLn "find all xoj files" 
  cdir <- getCurrentDirectory 
  findxoj machine dest cdir 


startConvertXoj :: IO () 
startConvertXoj = do 
  putStrLn "find all xoj file and convert"
  cdir <- getCurrentDirectory 
  shortpaths <- getDirectoryContents cdir 
  let fullpaths = map (cdir </>) shortpaths
  forM_ fullpaths $ \item -> do 
    checkFileExistAndDo item $ \fp -> do 
      status <- getFileStatus fp 
      when (isRegularFile status && not (isSymbolicLink status)) $  
        if checkXoj item 
          then convertXoj item 
          else return ()


convertXoj :: FilePath -> IO () 
convertXoj fp = do 
  let basename = takeBaseName fp 
  cdir <- getCurrentDirectory 
  createDirectory basename 
  putStrLn $ "xournal-convert makesvg --dest=" ++ cdir </> basename ++ " " ++ fp 
  system $ "xournal-convert makesvg --dest=" ++ cdir </> basename ++ " " ++ fp 
  return ()


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


checkFileExistAndDo :: FilePath -> (FilePath -> IO ()) -> IO ()
checkFileExistAndDo fp action = doesFileExist fp >>= \b -> when b (action fp)

checkDirExistAndDo :: FilePath -> (FilePath -> IO ()) -> IO ()
checkDirExistAndDo fp action = doesDirectoryExist fp >>= \b -> when b (action fp)


checkXoj :: FilePath -> Bool  
checkXoj fp = 
  let (fname_wo_ext,fname_ext) = splitExtension fp
  in  fname_ext == ".xoj" 
-- $ 
--     putStrLn $ " bingo = " ++ fp

copyXoj :: String -> FilePath -> FilePath -> IO ()
copyXoj prefix srcfile dest = 
  when (checkXoj srcfile) $ do
    let newname = mkNewXojName prefix srcfile 
    putStrLn $ "newname = " ++ newname
    system $ "cp -a " ++ srcfile ++ " " ++ (dest </> newname)
    return () 


findxoj :: String -> FilePath -> FilePath -> IO () 
findxoj prefix dest path = do  
  putStrLn $ " entering in the directory : " ++ path 
  shortpaths <- getDirectoryContents path 
  let fullpaths = map (path </>) shortpaths
  forM_ fullpaths $ \item -> do 
    checkFileExistAndDo item $ \fp -> do 
      status <- getFileStatus fp 
      when (isRegularFile status && not (isSymbolicLink status)) $
        copyXoj prefix fp dest 
  forM_ fullpaths $ \item -> do 
    subdiraction item (findxoj prefix dest)

mkNewXojName :: String -> FilePath -> FilePath 
mkNewXojName prefix fp = 
  let paths = splitPath fp
      filename = takeBaseName fp  
      dirs = filter (not.null) . map init . filter (\x -> last x == '/') 
             $ paths 
  in filename ++ "_" ++ prefix ++ "_" ++ intercalate "_" dirs <.> "xoj"

