import Control.Applicative
import Data.Function
import Data.Char
import Data.List
import HSH
import System.Environment
import System.FilePath.Posix
import System.IO
import qualified Data.ByteString as B

anyIsInfixOf :: (Eq a) => [[a]] -> [a] -> Bool
xs `anyIsInfixOf` y = or [x `isInfixOf` y | x <- xs]

main :: IO ()
main = do
  l <- B.split (fromIntegral $ ord '\n') <$> run "find ~/m/mus -type f"
  h <- getEnv "HOME"
  let
    c = sortBy (compare `on` takeBaseName) $
      map (map (chr . fromIntegral) . B.unpack) l
  writeFile (h ++ "/m/mus-all.m3u") $ unlines c
  writeFile (h ++ "/m/mus-danl.m3u") . unlines $
    filter (not . (["Six Cents and Natalie", "Tullycraft"] `anyIsInfixOf`)) c
