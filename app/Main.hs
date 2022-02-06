module Main where

import Control.Concurrent
import System.Process
import System.Directory
import System.Random
import System.Directory.Internal.Prelude (getArgs)

foo :: String -> Int -> IO ()
foo str n = do
  thread <- forkIO . callCommand $ "swaybg -i " <> str
  threadDelay $ n * 1000000
  killThread thread


randomElement :: RandomGen g => [a] -> g -> (a, g)
randomElement xs gen = let (n, newgen) = randomR (0, length xs) gen in (xs !! n, newgen)

randomImages :: [FilePath] -> StdGen -> IO ()
randomImages imgs gen = do
  let (randomPath, newgen) = randomElement imgs gen
  forkIO $ foo randomPath 10
  threadDelay $ 10 * 1000000 - 1000000
  randomImages imgs newgen

main :: IO ()
main = do
  (x:_) <- getArgs
  homeFix x >>= main'

-- | Takes a directory of images to randomise for background
main' :: FilePath -> IO ()
main' dir = do
  gen <- getStdGen
  setCurrentDirectory dir
  images <- listDirectory dir
  randomImages images gen

homeFix :: FilePath -> IO FilePath
homeFix "" = return ""
homeFix ('~':rest) = do
  home <- getHomeDirectory
  return $ home <> rest
homeFix xs = return xs
