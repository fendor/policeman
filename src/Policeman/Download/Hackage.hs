-- | Functions to download Haskell packages from Hackage.
module Policeman.Download.Hackage (
  PackageName (..),
  downloadFromHackage,
  getLatestHackageCabalFileContent,
) where

import Control.Exception (catch)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Policeman.Core.Package (PackageName (..))
import Policeman.Core.Version (Version (versionText))
import Policeman.Download.Common (DownloadError (..), evidenceDir)
import qualified System.Process as Process

{- | By the given pacakge name it downloads the latest `.cabal` file and
returns its content.

<http://hackage.haskell.org/package/<name>/<name>.cabal>
-}
getLatestHackageCabalFileContent ::
  PackageName ->
  ExceptT DownloadError IO ByteString
getLatestHackageCabalFileContent package@(PackageName packageName) =
  ExceptT $
    (Right . Encoding.encodeUtf8 <$> (readProcess "curl" ["--silent", url])) <|> pure (Left $ NoSuchPackage package)
 where
  url :: Text
  url =
    mconcat
      [ hackageUrl
      , packageName
      , "/"
      , packageName
      , ".cabal"
      ]

{- | This function takes 'PackageName' and previous package
'Version', downloads @.tar.gz@ archive from Hackage and unpacks it in
the current directory.
-}
downloadFromHackage :: PackageName -> Version -> ExceptT DownloadError IO FilePath
downloadFromHackage packageName@(PackageName name) (versionText -> version) = do
  let fullName = name <> "-" <> version
  let tarName = fullName <> ".tar.gz"
  let tarUrl =
        mconcat
          [ hackageUrl
          , name
          , "/"
          , tarName
          ]

  let tarPath = Text.pack $ evidenceDir </> Text.unpack tarName
  let srcPath = evidenceDir </> Text.unpack fullName
  liftIO $ createDirectoryIfMissing True evidenceDir
  withExceptT SystemError $ removeDirIfExists srcPath

  -- download archive from Hackage
  ExceptT $
    (Right <$> readProcess "curl" ["--silent", tarUrl, "--output", tarPath])
      <|> pure (Left $ NoSuchPackage packageName)

  -- unpack
  liftIO $ readProcess "tar" ["-xf", tarPath, "-C", Text.pack evidenceDir]
  liftIO $ fmap (</> srcPath) getCurrentDirectory

hackageUrl :: Text
hackageUrl = "http://hackage.haskell.org/package/"

removeDirIfExists :: FilePath -> ExceptT IOError IO ()
removeDirIfExists fileName =
  ExceptT $
    (Right <$> removeDirectoryRecursive fileName) `catch` (pure . handleExists)
 where
  handleExists :: IOError -> Either IOError ()
  handleExists e
    | isDoesNotExistError e = Right ()
    | otherwise = Left e

readProcess :: FilePath -> [Text] -> IO Text
readProcess fp args = Text.strip . Text.pack <$> Process.readProcess fp (map Text.unpack args) ""
