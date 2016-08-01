{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ((<$>),(<*>))
import Control.Monad (when,filterM)
import Control.Monad.Trans.Maybe 
import Data.Configurator as C
import qualified Data.List as L 
import Data.Maybe (catMaybes,isNothing) 
import Data.UUID.V4 (nextRandom)
import System.Directory -- (doesDirectoryExist, copyFile, getHomeDirectory, getTemporaryDirectory, removeFile)
import System.Console.CmdArgs
import System.Directory.Tree (AnchoredDirTree(..), DirTree(..), build, flattenDir)
import System.FilePath (makeRelative, splitFileName, takeExtension, (</>))
import System.Process (system,readProcess)

isHtml = ( == ".html") <$> takeExtension

isFile (File _ _) = True
isFile _ = False

takeFile x | isFile x = (Just . file) x 
takeFile x | otherwise = Nothing 

data HaddockReplace = Replace { buildpath :: String 
                              , config :: FilePath } 
                   deriving (Show,Data,Typeable) 

replace :: FilePath -> HaddockReplace 
replace dothaddock = 
  Replace { buildpath = def &= typ "TARGETFILEDIR" &= argPos 0 
          , config = dothaddock
          }

mode :: FilePath -> HaddockReplace 
mode dothaddock = modes [replace dothaddock]

prepareDirectory :: FilePath -> IO ()
prepareDirectory fp = do 
  let (dir,_) = splitFileName fp 
  b <- doesDirectoryExist dir 
  when (not b) $ system ("mkdir -p " ++ dir) >> return () 

processFile :: (FilePath,FilePath) -> String -> FilePath -> FilePath -> IO ()
processFile (doc1,doc2) url src tgt = do 
  uuid <- nextRandom
  tdir <- getTemporaryDirectory 
  let tfile = tdir </> (show uuid)
  readProcess "simplereplace" [ "+RTS", "-K100M", "-RTS"
                              , doc1, url, src, tfile] "" 
  readProcess "simplereplace" ["+RTS", "-K100M", "-RTS"
                              , doc2, url, tfile, tgt] "" 
  removeFile tfile
  


isUpdated :: (FilePath,FilePath) -> IO Bool 
isUpdated (ofp,nfp) = do 
  b <- doesFileExist nfp
  if not b 
    then return True
    else do 
      otime <- getModificationTime ofp
      ntime <- getModificationTime nfp 
      return (otime > ntime)


main :: IO () 
main = do 
  dothaddock <- (</> ".haddock") <$> getHomeDirectory
  params <- cmdArgs (mode dothaddock)

  cfg <- load [Required (config params)] 
  Just (rootpath,urlbase,doc1,doc2) :: Maybe (FilePath,FilePath,FilePath,FilePath)
    <- runMaybeT $ do 
         (,,,) <$> MaybeT (C.lookup cfg "rootpath")
               <*> MaybeT (C.lookup cfg "urlbase")
               <*> MaybeT (C.lookup cfg "doc1")
               <*> MaybeT (C.lookup cfg "doc2")

  (r :/ r') <- build rootpath
  let files = catMaybes . map takeFile . flattenDir $ r' 
      -- htmlfiles = filter isHtml files 
      pairs = map ((,) <$> id
                   <*> (buildpath params </>) . makeRelative rootpath) 
                  files 
      swappedpairs = map (\(x,y)->(y,x)) pairs 

  (b :/ b') <- build (buildpath params)
  let filesInTarget = catMaybes . map takeFile . flattenDir $ b' 
      willbeerased = filter (\x -> isNothing (L.lookup x swappedpairs )) filesInTarget
  mapM_ removeFile willbeerased 
  updatedpairs <- filterM isUpdated pairs 

  -- mapM_ print updatedpairs 
  mapM_ (prepareDirectory . snd) updatedpairs 
  mapM_ (\x -> print x >> uncurry (processFile (doc1,doc2) urlbase) x) updatedpairs 

