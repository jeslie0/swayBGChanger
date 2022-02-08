module Main where

import Control.Concurrent
import System.Process
import System.Exit
import System.Directory
import System.Random
import System.Directory.Internal.Prelude (getArgs)

-- | Runs the swaybg command for n seconds, then kills the thread.
foo :: String -> Int -> IO ()
foo str n = do
  thread <- forkIO . callCommand $ "swaybg -i " <> str
  threadDelay $ n * 1000000
  killThread thread

-- | Gives a random element from a list, and a new generator.
randomElement :: RandomGen g => [a] -> g -> (a, g)
randomElement xs gen = let (n, newgen) = randomR (0, length xs - 1) gen in (xs !! n, newgen)

-- | Given a list of images, one is randomly selected, then displayed
-- on a new thread by "foo" for 10 seconds. It then loops just before
-- the time runs out
randomImages :: [FilePath] -> StdGen -> IO ()
randomImages imgs gen = do
  let (randomPath, newgen) = randomElement imgs gen
  forkIO $ do
    foo randomPath 10
    print "Thread ending"
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
  runWhenSwayIsAlive $ randomImages images gen

homeFix :: FilePath -> IO FilePath
homeFix "" = return ""
homeFix ('~':rest) = do
  home <- getHomeDirectory
  return $ home <> rest
homeFix xs = return xs

isSwayRunning :: IO Bool
isSwayRunning = do
  (exitcode, _, _) <- readProcessWithExitCode "pidof" ["sway"] ""
  case exitcode of
    ExitSuccess -> return True
    ExitFailure _ -> return False

runWhenSwayIsAlive :: IO () -> IO ()
runWhenSwayIsAlive prog = do
  thread <- forkIO prog
  test thread
    where
      test :: ThreadId -> IO ()
      test threadid = do
        bool <- isSwayRunning
        if bool
          then do
          threadDelay 1000000
          test threadid
          else
          killThread threadid
