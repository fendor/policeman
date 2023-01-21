-- | Functions to work with Cabal file and its content.
module Policeman.Cabal (
  CabalError (..),
  parseCabalFile,
  findCabalDescription,
  extractExposedModules,
  extractPackageName,
  extractPackageVersion,
) where

import Control.Monad.Except (ExceptT, throwError)
import Distribution.ModuleName (ModuleName, components)
import Distribution.PackageDescription (
  CondTree (..),
  GenericPackageDescription (..),
  Library (..),
  PackageDescription (..),
 )
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.Version (versionNumbers)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import Policeman.Core.Package (Module (..), PackageName (..))
import Policeman.Core.Version (Version, versionFromIntList)

import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import qualified Data.Text as Text
import qualified Distribution.Types.PackageName as Cabal

-- | Errors about dealing with @.cabal@ files.
data CabalError
  = -- | Directory where we're looking for cabal file, not found.
    NoSuchDirectory FilePath
  | -- | No @.cabal@ file under given path.
    NoCabalFile FilePath
  | -- | Multiple cabal files found.
    MultipleCabalFiles (NonEmpty FilePath)
  | -- | Error parsing cabal file.
    CabalParseError
  deriving stock (Show, Eq)

{- | This function takes a path to a directory, finds a cabal file
under this directory, parses it and returns parsed content.
-}
findCabalDescription :: FilePath -> ExceptT CabalError IO GenericPackageDescription
findCabalDescription dirPath = do
  unlessM (liftIO $ doesDirectoryExist dirPath) $ throwError $ NoSuchDirectory dirPath
  dirContent <- liftIO $ listDirectory dirPath
  let cabalFiles = filter (\p -> takeExtension p == ".cabal") dirContent
  case cabalFiles of
    [] -> throwError $ NoCabalFile dirPath
    [cabalFile] -> do
      let cabalPath = dirPath </> cabalFile
      cabalContent <- liftIO $ BS.readFile cabalPath
      parseCabalFile cabalContent
    x : xs -> throwError $ MultipleCabalFiles (x NE.:| xs)

-- | Parses the given cabal file source and returns 'GenericPackageDescription'
parseCabalFile :: ByteString -> ExceptT CabalError IO GenericPackageDescription
parseCabalFile cabalContent = case parseGenericPackageDescriptionMaybe cabalContent of
  Nothing -> throwError CabalParseError -- TODO: better error
  Just gpd -> pure gpd

extractPackageName :: GenericPackageDescription -> PackageName
extractPackageName =
  PackageName
    . Text.pack
    . show
    . Cabal.unPackageName
    . pkgName
    . package
    . packageDescription

extractPackageVersion :: GenericPackageDescription -> Maybe Version
extractPackageVersion =
  versionFromIntList
    . versionNumbers
    . pkgVersion
    . package
    . packageDescription

extractExposedModules :: GenericPackageDescription -> [Module]
extractExposedModules =
  map toModule
    . concatMap (exposedModules . condTreeData)
    . maybeToList
    . condLibrary
 where
  toModule :: ModuleName -> Module
  toModule = Module . Text.pack . intercalate "." . components
