module Main where

import Control.Concurrent
import Control.Exception (displayException, try)
import Control.Monad
import Data.Maybe
import System.Console.Haskeline
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process


main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  args <- getArgs
  let args' = case length args of
                0 -> [ "dev", currentDir ]
                1 -> args ++ [ currentDir ]
                _ -> take 2 args
      [ mixEnv, workingDir ] = args'
  eitherDirContents <- try $ (getDirectoryContents workingDir) :: IO (Either SomeException [FilePath])
  case eitherDirContents of
    Left err -> putStrLn $ (displayException err) ++ "\n"
    Right workingDirContents
      | "mix.exs" `elem` workingDirContents -> do
          -- Set environment variables
          let envVars = if mixEnv /= "dev"
                          then [ ("MIX_ENV", mixEnv),
                                 ("DEEP_BLUE_SSL_KEY_PATH", "/Users/davidescobar/Downloads/wildcard_lendstreet_com_pkg/lendstreet.com.key"),
                                 ("DEEP_BLUE_SSL_CERT_PATH", "/Users/davidescobar/Downloads/wildcard_lendstreet_com_pkg/lendstreet.com.crt") ]
                          else [ ("MIX_ENV", mixEnv) ]
              historyFilePath = workingDir </> "iex2.log"
          mapM_ (\(var, value) -> setEnv var value) envVars

          (hIn, hOut, hErr, pid) <- runInteractiveProcess "iex" [ "-S", "mix" ] (Just workingDir) Nothing
          hSetBinaryMode hIn False
          hSetBinaryMode hOut False
          hSetBuffering hIn LineBuffering
          hSetBuffering hOut NoBuffering
          hSetBuffering stdout NoBuffering
          runInteractiveConsole hIn hOut hErr pid historyFilePath
      | otherwise -> putStrLn $ "Could not find a mix.exs file in '" ++ workingDir ++ "'.\n"


runInteractiveConsole :: Handle -> Handle -> Handle -> ProcessHandle -> FilePath -> IO ()
runInteractiveConsole hIn hOut hErr pid historyFilePath = do
  _ <- forkIO $ runOutputLoop pid hOut hErr
  _ <- forkIO $ runInputLoop pid hIn historyFilePath
  forever $ do
    threadDelay 1000000 -- 1 second
    exitCodeMaybe <- getProcessExitCode pid
    when (isJust exitCodeMaybe) (exitWith $ fromJust exitCodeMaybe)


runInputLoop :: ProcessHandle -> Handle -> FilePath -> IO ()
runInputLoop pid hIn historyFilePath = do
  let settings = defaultSettings { historyFile = Just historyFilePath }
  maybeLine <- runInputT settings (getInputLine "")
  success <- try $ hPutStrLn hIn (fromMaybe "" maybeLine) :: IO (Either SomeException ())
  case success of
    Left err -> putStrLn $ displayException err
    Right _ -> runInputLoop pid hIn historyFilePath


runOutputLoop :: ProcessHandle -> Handle -> Handle -> IO ()
runOutputLoop pid hOut hErr = do
  contentsOrError <- try $ hGetContents hOut :: IO (Either SomeException String)
  case contentsOrError of
    Left contErr -> do
      putStrLn $ displayException contErr
      errorContentsOrError <- try $ hGetContents hErr :: IO (Either SomeException String)
      case errorContentsOrError of
        Left errContErr -> putStrLn $ displayException errContErr
        Right errorContents -> putStrLn errorContents
    Right contents -> do
      putStrLn contents
      runOutputLoop pid hOut hErr
