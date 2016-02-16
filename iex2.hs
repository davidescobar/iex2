module Main where

import Control.Concurrent
import Control.Exception (displayException, try)
import Control.Monad
import Data.Int
import Data.List
import Data.Maybe
import Data.String.Utils
import System.Console.ANSI
import System.Console.Haskeline
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

import qualified Data.Map as Map
import qualified System.Posix as Posix


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
          sslFilePaths <- getSSLPaths workingDir
          let crtFilePath = fromMaybe "" (Map.lookup "crt" sslFilePaths)
              keyFilePath = fromMaybe "" (Map.lookup "key" sslFilePaths)
              envVars = if mixEnv /= "dev"
                          then [ ("MIX_ENV", mixEnv),
                                 ("DEEP_BLUE_SSL_KEY_PATH", crtFilePath),
                                 ("DEEP_BLUE_SSL_CERT_PATH", keyFilePath) ]
                          else [ ("MIX_ENV", mixEnv) ]
              historyFilePath = workingDir </> ".iex2.log"
          mapM_ (\(var, value) -> setEnv var value) envVars

          (hIn, hOut, hErr, pid) <- runInteractiveProcess "iex" [ "-S", "mix" ] (Just workingDir) Nothing
          hSetBinaryMode hIn False
          hSetBinaryMode hOut False
          hSetBuffering hIn NoBuffering
          hSetBuffering hOut NoBuffering
          hSetBuffering stdout NoBuffering
          runInteractiveConsole hIn hOut hErr pid historyFilePath
      | otherwise -> putStrLn $ "Could not find a mix.exs file in '" ++ workingDir ++ "'.\n"


getSSLPaths :: String -> IO (Map.Map String String)
getSSLPaths iexDir = do
  iexDirExists <- doesDirectoryExist iexDir
  if iexDirExists
    then do
      let pathsYAMLFile = iexDir </> "config/paths.yml"
      yamlLines <- readFile pathsYAMLFile >>= return . lines
      let crtFileMap = case find (isInfixOf "crt:") yamlLines of
                         Just line -> Map.fromList [ ("crt", strip $ last $ split ":" line) ]
                         Nothing -> Map.empty
      let keyFileMap = case find (isInfixOf "key:") yamlLines of
                          Just line -> Map.fromList [ ("key", strip $ last $ split ":" line) ]
                          Nothing -> Map.empty
      return $ Map.union crtFileMap keyFileMap
    else return Map.empty


reduceLogFileSize :: Int64 -> FilePath -> IO ()
reduceLogFileSize maxSizeLimit historyFilePath = do
  exists <- doesFileExist historyFilePath
  if exists
    then do
      currentSize <- Posix.getFileStatus historyFilePath >>= return . fromIntegral . Posix.fileSize
      if currentSize > maxSizeLimit
        then do
          logFileLines <- readFile historyFilePath >>= return . lines
          let newNumLines = (length logFileLines) `div` 2
              newHistoryFile = (takeDirectory historyFilePath) </> ".new_iex2.log"
              newLogContent = unlines $ take newNumLines logFileLines
          writeFile newHistoryFile newLogContent
          removeFile historyFilePath
          renameFile newHistoryFile historyFilePath
        else return ()
    else return ()


runInteractiveConsole :: Handle -> Handle -> Handle -> ProcessHandle -> FilePath -> IO ()
runInteractiveConsole hIn hOut hErr pid historyFilePath = do
  reduceLogFileSize (2^20) historyFilePath
  _ <- forkIO $ runOutputLoop hOut
  _ <- forkIO $ runInputLoop hIn historyFilePath
  _ <- forkIO $ runErrorLoop hErr
  forever $ do
    threadDelay 1000000 -- 1 second
    exitCodeMaybe <- getProcessExitCode pid
    when (isJust exitCodeMaybe) (exitWith $ fromJust exitCodeMaybe)


runInputLoop :: Handle -> FilePath -> IO ()
runInputLoop hIn historyFilePath = do
  let settings = defaultSettings { historyFile = Just historyFilePath }
  maybeLine <- runInputT settings (getInputLine "")
  success <- try $ hPutStrLn hIn (fromMaybe "" maybeLine) :: IO (Either SomeException ())
  case success of
    Left err -> putStrLn $ displayException err
    Right _ -> runInputLoop hIn historyFilePath


runOutputLoop :: Handle -> IO ()
runOutputLoop hOut = do
  hOutReady <- hReady hOut
  if hOutReady
    then do
      setSGR [ Reset ]
      contentsOrErr <- try $ hGetContents hOut :: IO (Either SomeException String)
      case contentsOrErr of
        Left contErr -> putStrLn $ displayException contErr
        Right contents -> do
          putStrLn contents
          runOutputLoop hOut
    else runOutputLoop hOut


runErrorLoop :: Handle -> IO ()
runErrorLoop hErr = do
  hErrReady <- hReady hErr
  if hErrReady
    then do
      setSGR [ SetColor Foreground Vivid Red ]
      eitherErr <- try $ hGetContents hErr :: IO (Either SomeException String)
      case eitherErr of
        Left err -> putStrLn $ displayException err
        Right errContents -> putStrLn errContents
      setSGR [ Reset ]
    else runErrorLoop hErr
