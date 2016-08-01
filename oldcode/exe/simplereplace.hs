{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative 
import           Data.Attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List 
import           Data.Monoid
import           System.Environment
import           System.IO 
--
import Prelude hiding (take) 

main :: IO () 
main = do 
  args <- getArgs 
  if length args /= 4
    then putStrLn "simplereplace originalstr newstr srcfile tgtfile" 
    else do 
      bstr <- B.readFile (args !! 2)
      let r = parseOnly (findparser (C.pack (args !! 0))) bstr 
      case r of 
        Left err -> print err 
        Right lst -> let nlst = intersperse (C.pack (args !! 1)) lst 
                     in B.writeFile (args !! 3) (mconcat nlst)


findparser :: B.ByteString -> Parser [B.ByteString]  
findparser bstr = do 
   strs <- many $  manyTill AC.anyChar (try (AC.string bstr)) 
   str <- manyTill AC.anyChar endOfInput 
   return $ (map C.pack strs) ++ [C.pack str] 
