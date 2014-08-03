
import Control.Applicative
import System.Directory
import System.Process
import Data.List
import System.Random

{-dir = "/home/russ/test/"-}
{-recentdir = "/home/russ/test/recent/"-}
dir = "/home/russ/Wallpapers/"
recentdir = "/home/russ/Wallpapers/Recent/"

main = do
    r <- clean $ getDirectoryContents dir
    if null r then reset else return () >> do
        bg <- pickrand r
        move (recentdir ++ bg, dir ++ bg)
        setwp (recentdir ++ bg)
    return ()

pickrand :: [a] -> IO (a)
pickrand x = do
    pos <- getStdRandom $ randomR (0, (length x)-1)    
    return (x!!pos)

clean :: IO [FilePath] -> IO [FilePath]
clean ls = (liftA $ filter test) ls
    where test f = or $ map (\p -> p f) (fmap isInfixOf [".png", ".jpg", ".jpeg"])

reset :: IO ()
reset = do
    files <- clean $ getDirectoryContents recentdir

    movetodir recentdir dir files

setwp :: String -> IO ()
setwp wp = createProcess (proc "feh" ["--bg-scale", wp]) >> return ()

movetodir :: String -> String -> [FilePath] -> IO ()
movetodir srcdir tgtdir list = mapM_ move $ zip targets sources
    where
        targets = map (tgtdir ++) list
        sources = map (srcdir ++) list

move :: (FilePath, FilePath) -> IO ()
move (target, source) = copyFile source target >> rmfile
    where rmfile = doesFileExist target >>= \x 
                    -> if x then removeFile source
                    else return ()


