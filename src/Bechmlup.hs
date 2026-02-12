{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bechmlup (run) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.List
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Options.Applicative
import System.Directory
import System.FilePath
import System.Info
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

-- | Tools managed by bechmlup
data Tool = Bechml | Roux
  deriving (Show, Eq, Enum, Bounded)

allTools :: [Tool]
allTools = [minBound .. maxBound]

toolName :: Tool -> String
toolName Bechml = "bechml"
toolName Roux = "roux"

toolRepo :: Tool -> String
toolRepo Bechml = "bechml/bechml-nightly"
toolRepo Roux = "bechml/roux-nightly"

-- | CLI commands
data Command
  = Install [Tool]
  | Uninstall [Tool]
  | UninstallAll
  | List
  deriving (Show)

-- | GitHub release info
data Release = Release
  { relTagName :: String,
    relAssets :: [Asset]
  }
  deriving (Show)

data Asset = Asset
  { assetName :: String,
    assetUrl :: String
  }
  deriving (Show)

instance FromJSON Release where
  parseJSON = withObject "Release" $ \o ->
    Release
      <$> o .: "tag_name"
      <*> o .: "assets"

instance FromJSON Asset where
  parseJSON = withObject "Asset" $ \o ->
    Asset
      <$> o .: "name"
      <*> o .: "browser_download_url"

-- | Root directory: ~/.bechmlup
bechmlupDir :: IO FilePath
bechmlupDir = do
  home <- getHomeDirectory
  pure $ home </> ".bechmlup"

-- | Bin directory: ~/.bechmlup/bin
binDir :: IO FilePath
binDir = do
  root <- bechmlupDir
  pure $ root </> "bin"

-- | Version file: ~/.bechmlup/versions/<tool>
versionFile :: Tool -> IO FilePath
versionFile tool = do
  root <- bechmlupDir
  let dir = root </> "versions"
  createDirectoryIfMissing True dir
  pure $ dir </> toolName tool

githubGet :: String -> IO BL.ByteString
githubGet url = do
  mgr <- newManager tlsManagerSettings
  req <- setUserAgent <$> parseRequest url
  resp <- httpLbs req mgr
  let code = statusCode (responseStatus resp)
  if code == 200
    then pure (responseBody resp)
    else fail $ "GitHub API request failed (" ++ show code ++ "): " ++ url

setUserAgent :: Request -> Request
setUserAgent req =
  req {requestHeaders = ("User-Agent", "bechmlup/0.1.0") : requestHeaders req}

fetchLatestRelease :: Tool -> IO Release
fetchLatestRelease tool = do
  -- Use /releases (not /releases/latest) so prereleases are included
  let url = "https://api.github.com/repos/" ++ toolRepo tool ++ "/releases?per_page=1"
  body <- githubGet url
  case eitherDecode body of
    Left err -> fail $ "Failed to parse release JSON for " ++ toolName tool ++ ": " ++ err
    Right [] -> fail $ "No releases found for " ++ toolName tool
    Right (r : _) -> pure r

-- | Pick the right asset for the current platform
pickAsset :: Release -> Maybe Asset
pickAsset Release {..} =
  let match = find (\a -> platformSuffix `isInfixOfCI` assetName a) relAssets
   in match `orElse` (if length relAssets == 1 then Just (head relAssets) else Nothing)
  where
    isInfixOfCI needle haystack =
      lowerS needle `isInfixOf'` lowerS haystack
    isInfixOf' [] _ = True
    isInfixOf' _ [] = False
    isInfixOf' n h = n `isPrefixOf` h || isInfixOf' n (tail h)
    lowerS = map toLowerC
    orElse Nothing y = y
    orElse x _ = x

platformSuffix :: String
platformSuffix = case os of
  "mingw32" -> "windows"
  "darwin" -> "macos"
  _ -> "linux"

downloadAsset :: Asset -> FilePath -> IO ()
downloadAsset asset dest = do
  mgr <- newManager tlsManagerSettings
  req <- setUserAgent <$> parseRequest (assetUrl asset)
  resp <- httpLbs req mgr
  let code = statusCode (responseStatus resp)
  unless (code == 200) $
    fail $
      "Download failed (" ++ show code ++ "): " ++ assetUrl asset
  BL.writeFile dest (responseBody resp)

-- | Check if an asset name looks like an archive
isArchive :: String -> Bool
isArchive name =
  ".tar.gz" `isSuffixOf` lname
    || ".tgz" `isSuffixOf` lname
    || ".zip" `isSuffixOf` lname
  where
    lname = map toLowerC name

-- | Extract an archive into a directory
extractArchive :: FilePath -> FilePath -> IO ()
extractArchive archive destDir
  | ".tar.gz" `isSuffixOf` lname || ".tgz" `isSuffixOf` lname =
      callProcess "tar" ["xzf", archive, "-C", destDir]
  | ".zip" `isSuffixOf` lname = do
      -- Use tar on Windows (available in modern Windows), fallback-friendly
      if os == "mingw32"
        then callProcess "tar" ["xf", archive, "-C", destDir]
        else callProcess "unzip" ["-o", "-q", archive, "-d", destDir]
  | otherwise =
      fail $ "Unknown archive format: " ++ archive
  where
    lname = map toLowerC archive

-- | Recursively find a file by name in a directory
findFileRecursive :: String -> FilePath -> IO (Maybe FilePath)
findFileRecursive target dir = do
  entries <- listDirectory dir
  let check [] = pure Nothing
      check (e : es) = do
        let full = dir </> e
        isDir <- doesDirectoryExist full
        if isDir
          then do
            found <- findFileRecursive target full
            case found of
              Just p -> pure (Just p)
              Nothing -> check es
          else
            if e == target || e == target <.> "exe"
              then pure (Just full)
              else check es
  check entries

installTool :: Tool -> IO ()
installTool tool = do
  let name = toolName tool

  -- Check if already installed
  vf <- versionFile tool
  installed <- doesFileExist vf
  when installed $ do
    current <- readFile vf
    putStrLn $ name ++ " currently at " ++ current ++ ", upgrading..."

  putStrLn $ "Fetching latest release for " ++ name ++ "..."
  rel <- fetchLatestRelease tool

  bin <- binDir
  createDirectoryIfMissing True bin

  let exeName = if os == "mingw32" then name ++ ".exe" else name
      destPath = bin </> exeName

  case pickAsset rel of
    Just asset -> do
      putStrLn $ "  Downloading " ++ assetName asset ++ " (" ++ relTagName rel ++ ")..."
      if isArchive (assetName asset)
        then withSystemTempDirectory "bechmlup" $ \tmpDir -> do
          let archivePath = tmpDir </> assetName asset
          downloadAsset asset archivePath
          putStrLn "  Extracting..."
          extractArchive archivePath tmpDir
          found <- findFileRecursive name tmpDir
          case found of
            Just exePath -> do
              copyFile exePath destPath
              makeExecutable destPath
              putStrLn $ "  Installed " ++ name ++ " " ++ relTagName rel ++ " to " ++ destPath
            Nothing ->
              fail $ "Could not find " ++ name ++ " executable in archive " ++ assetName asset
        else do
          downloadAsset asset destPath
          makeExecutable destPath
          putStrLn $ "  Installed " ++ name ++ " " ++ relTagName rel ++ " to " ++ destPath
    Nothing ->
      fail $ "No matching asset for platform '" ++ platformSuffix ++ "' in release " ++ relTagName rel

  -- Record installed version
  writeFile vf (relTagName rel)
  putStrLn $ "  " ++ name ++ " " ++ relTagName rel ++ " recorded."

uninstallTool :: Tool -> IO ()
uninstallTool tool = do
  bin <- binDir
  let name = toolName tool
      exeName = if os == "mingw32" then name ++ ".exe" else name
      path = bin </> exeName
  exists <- doesFileExist path
  when exists $ do
    removeFile path
    putStrLn $ "Removed " ++ path
  vf <- versionFile tool
  vfExists <- doesFileExist vf
  when vfExists $ do
    removeFile vf
    putStrLn $ "Removed version record for " ++ name
  unless (exists || vfExists) $
    putStrLn $
      name ++ " is not installed."

uninstallAll :: IO ()
uninstallAll = do
  putStrLn "Uninstalling all tools managed by bechmlup..."
  forM_ allTools uninstallTool
  root <- bechmlupDir
  exists <- doesDirectoryExist root
  when exists $ do
    removeDirectoryRecursive root
    putStrLn $ "Removed " ++ root
  putStrLn "bechmlup has been fully uninstalled."
  putStrLn "You may also want to remove bechmlup from your PATH."

listInstalled :: IO ()
listInstalled = do
  putStrLn "Installed tools:"
  forM_ allTools $ \tool -> do
    vf <- versionFile tool
    installed <- doesFileExist vf
    if installed
      then do
        ver <- readFile vf
        putStrLn $ "  " ++ toolName tool ++ " " ++ ver
      else
        putStrLn $ "  " ++ toolName tool ++ " (not installed)"

makeExecutable :: FilePath -> IO ()
makeExecutable path = do
  p <- getPermissions path
  setPermissions path (setOwnerExecutable True p)

toLowerC :: Char -> Char
toLowerC c
  | isAsciiUpper c = toEnum (fromEnum c + 32)
  | otherwise = c

parseCommand :: ParserInfo Command
parseCommand =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> header "bechmlup - installer for bechml and roux"
        <> progDesc "Install, upgrade, and manage bechml toolchain components"
    )

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "install"
        (info installP (progDesc "Install or upgrade tools (default: all)"))
        <> command
          "uninstall"
          (info uninstallP (progDesc "Uninstall tools"))
        <> command
          "uninstall-all"
          (info (pure UninstallAll) (progDesc "Uninstall everything including bechmlup"))
        <> command
          "list"
          (info (pure List) (progDesc "List installed tools and versions"))
    )

installP :: Parser Command
installP = Install <$> toolsArg

uninstallP :: Parser Command
uninstallP = Uninstall <$> toolsArg

toolsArg :: Parser [Tool]
toolsArg =
  fmap collapse $
    many $
      argument
        readTool
        ( metavar "TOOL..."
            <> help "Tools to operate on: bechml, roux (default: all)"
        )
  where
    collapse [] = allTools
    collapse ts = ts

readTool :: ReadM Tool
readTool = eitherReader $ \s -> case map toLowerC s of
  "bechml" -> Right Bechml
  "roux" -> Right Roux
  _ -> Left $ "Unknown tool: " ++ s ++ ". Available: bechml, roux"

runCommand :: Command -> IO ()
runCommand (Install tools) = mapM_ installTool tools
runCommand (Uninstall tools) = mapM_ uninstallTool tools
runCommand UninstallAll = uninstallAll
runCommand List = listInstalled

run :: IO ()
run = execParser parseCommand >>= runCommand
