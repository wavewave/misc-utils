import Control.Lens
import Data.Default
import Data.List
import Data.List.Split
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Plot
import Graphics.Rendering.Chart.Backend.Cairo
import System.Directory
import System.Process
--
import Debug.Trace

isCommitLine x = take 6 x == "commit" 

isAuthorLine x = take 6 x == "Author"

isDateLine x = take 4 x == "Date"


data AuthDate = AuthDate { author :: String
                         , date :: Day -- String
                         }
                deriving Show

instance Default AuthDate where
  def = AuthDate "" (toEnum 0)
                         
extractAuthor s x = if isAuthorLine x
                    then s { author = drop 8 x }
                    else s  

extractDate s x = if isDateLine x
                  then s { date = ptime (drop 8 x) }
                  else s  



ptime = parseTimeOrError False defaultTimeLocale "%F %T %z" -- "%a %b %e %T %Y"

dirs = [ "nlp-prototype", "ygp-sync", "ygp-webapp" ]  

getStats :: FilePath -> IO [AuthDate]
getStats fp = do
  cwd <- getCurrentDirectory
  setCurrentDirectory fp
  result <- readCreateProcess (shell "git log --date=iso-local") ""
  let xs = lines result
      xss = splitWhen isCommitLine xs
  setCurrentDirectory cwd
  return $ map (foldl' (\s x -> extractDate (extractAuthor s x) x) def) . filter (not.null) $ xss

main :: IO ()
main = do
  lst0 <- concat <$> mapM getStats dirs

  let lst = lst0
  -- let lst = filter ((==) "Ian-Woo Kim <ianwookim@gmail.com>" . author) lst0
  
  let hist :: PlotHist Double Int
      hist =
              (plot_hist_values .~ map (fromIntegral . toModifiedJulianDay . date) lst )
              def
  toFile def "test.png" $ do
    layout_plots .= [ histToPlot hist ] 

