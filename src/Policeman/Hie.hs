module Policeman.Hie
    ( createHieFiles
    , readHieFiles
    ) where


import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.Directory.Recursive (getDirRecursive)
import System.FilePath ((</>))
import GHC.Iface.Ext.Types
import GHC.Types.Name.Cache
import GHC.Iface.Ext.Binary
import Control.Monad (filterM, forM, void)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Process as Process


{- | Generates HIE files for the project in the given folder and
returns their parsed content. The generated files are going to be
created in the @.hie/@ folder inside the given directory.
-}
createHieFiles :: FilePath -> IO [HieFile]
createHieFiles projectDir = do
    curDir <- getCurrentDirectory
    setCurrentDirectory projectDir

    -- make sure cabal isn't confused by any other "cabal.project"
    -- file in a parent directory
    -- See discussion: https://github.com/kowainik/policeman/issues/52
    writeFile "cabal.project" "packages: ."

    readProcess_ "cabal" ["clean"]
    readProcess_ "cabal"
        [ "v2-build"
        , "--ghc-options=-fwrite-ide-info"
        , "--ghc-options=-hiedir=.hie"
        , "--verbose=0"
        ]
    readHieFiles <* setCurrentDirectory curDir

{- | Returns content of all @.hie@ files recursively in the current
directory from the @.hie/@ folder.
-}
readHieFiles :: IO [HieFile]
readHieFiles = do
    hieDir <- (</> ".hie") <$> getCurrentDirectory
    nameCache <- initNameCache 'z' []
    hieContent <- getDirRecursive hieDir
    hieFiles <- filterM doesFileExist hieContent
    forM hieFiles $ \hiePath -> do
        hieFileResult <- readHieFile nameCache hiePath
        pure $ hie_file_result hieFileResult

readProcess_ :: FilePath -> [Text] -> IO ()
readProcess_ fp args = void $ Process.readProcess fp (map Text.unpack args) ""
