module Main where

import Control.Concurrent
import System.Directory
import System.Directory.Internal.Prelude (getArgs)
import System.Exit
import System.Process
import System.Random

-- * Random Functions

-- | Gives a random element from a list, and a new generator.
randomElement :: RandomGen g => [a] -> g -> (a, g)
randomElement xs gen =
  let (n, newgen) = randomR (0, length xs - 1) gen
   in (xs !! n, newgen)

-- * Sway Functions

-- | Runs the swaybg command for n seconds, then kills the thread.
swaybgCommand :: String -> Int -> IO ()
swaybgCommand str n = do
  thread <- forkIO . callCommand $ "swaybg -i " <> str
  threadDelay $ n * 1000000
  killThread thread

-- | Determines if Sway is running on the system by checking its
-- PID. Note that this isn't perfect - there could be another user
-- with Sway running.
isSwayRunning :: IO Bool
isSwayRunning = do
  (exitcode, _, _) <- readProcessWithExitCode "pidof" ["sway"] ""
  case exitcode of
    ExitSuccess -> return True
    ExitFailure _ -> return False

-- | Given a program of type IO (), it is run on a separate thread, so
-- long as Sway is also running. The test for whether or not Sway is
-- running occurs once per second.
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
        else killThread threadid

-- | Given a list of images, one is randomly selected, then displayed
-- on a new thread by "swaybgCommand" for n seconds. It then loops just before
-- the time runs out.
randomImages :: [FilePath] -> Int -> StdGen -> IO ()
randomImages imgs n gen = do
  let (randomPath, newgen) = randomElement imgs gen
  forkIO $ do swaybgCommand randomPath n
  threadDelay $ n * 1000000 - 500000
  randomImages imgs n newgen

-- * Miscelaneous functions
-- | Replaces a leading '~' in a file path with the home directory. If
-- there isn't a leading '~', the output is unchanged.
homeFix :: FilePath -> IO FilePath
homeFix "" = return ""
homeFix ('~' : rest) = do
  home <- getHomeDirectory
  return $ home <> rest
homeFix xs = return xs

-- * Main
-- | Takes system arguments of a directory of images and a number of
-- seconds. The images are then randomly shown as a wallpaper,
-- changing after the given number of seconds has elapsed.
main :: IO ()
main = do
  (x : y : _) <- getArgs
  let n = read y :: Int
  homeFix x >>= (\p -> main' p n)
  where
    main' :: FilePath -> Int -> IO ()
    main' dir n = do
      gen <- getStdGen
      setCurrentDirectory dir
      images <- listDirectory dir
      runWhenSwayIsAlive $ randomImages images n gen
