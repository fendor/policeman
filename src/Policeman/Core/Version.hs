{- | Module for 'Version' data type and helper functions to work with the
PVP-agreed versioning.

More information about PVP: <https://pvp.haskell.org/>.
-}
module Policeman.Core.Version (
  Version (..),

  -- ** Parse/Display helpers
  versionToText,
  versionFromText,
  versionToIntList,
  versionFromIntList,

  -- ** Bumping versions
  bumpMarketingMajorVersion,
  bumpMajorVersion,
  bumpMinorVersion,
  bumpTinyMinorVersion,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

-- | Package version that should satisfy the PVP.
data Version = Version
  { versionA :: !Int
  -- ^ Marketing major.
  , versionB :: !Int
  -- ^ Major.
  , versionC :: !Int
  -- ^ Minor.
  , versionD :: !Int
  -- ^ Tiny minor.
  , versionText :: !Text
  -- ^ Original text version.
  }
  deriving stock (Eq, Show)

-- | Initial minimal PVP-satisfactory version: @0.0.0.0@.
nullVersion :: Version
nullVersion =
  Version
    { versionA = 0
    , versionB = 0
    , versionC = 0
    , versionD = 0
    , versionText = ""
    }

{- | Shows 'Version' in the standard format:

@
A.B.C.D
1.0.2.1
@
-}
versionToText :: Version -> Text
versionToText Version{..} =
  Text.intercalate "." $
    map
      (Text.pack . show)
      [ versionA
      , versionB
      , versionC
      , versionD
      ]

versionFromText :: Text -> Maybe Version
versionFromText (Text.strip -> txt) =
  maybeInts >>= \ints ->
    if length ints > 4
      then Nothing
      else (\ver -> ver{versionText = txt}) <$> versionFromIntList ints
 where
  maybeInts :: Maybe [Int]
  maybeInts = mapM (readMaybe @Int . Text.unpack) $ Text.split (== '.') txt

versionToIntList :: Version -> [Int]
versionToIntList Version{..} =
  [ versionA
  , versionB
  , versionC
  , versionD
  ]

-- TODO: check on negative numbers
versionFromIntList :: [Int] -> Maybe Version
versionFromIntList [] = Nothing
versionFromIntList l@(x : xs) = Just $ mkVer x (toTriple xs)
 where
  mkVer :: Int -> (Int, Int, Int) -> Version
  mkVer versionA (versionB, versionC, versionD) =
    Version
      { versionText = Text.intercalate "." $ map (Text.pack . show) l
      , ..
      }

  toTriple :: [Int] -> (Int, Int, Int)
  toTriple (a : b : c : _rest) = (a, b, c)
  toTriple [a, b] = (a, b, 0)
  toTriple [a] = (a, 0, 0)
  toTriple [] = (0, 0, 0)

bumpMarketingMajorVersion :: Version -> Version
bumpMarketingMajorVersion Version{..} =
  nullVersion
    { versionA = versionA + 1
    }

bumpMajorVersion :: Version -> Version
bumpMajorVersion Version{..} =
  nullVersion
    { versionA = versionA
    , versionB = versionB + 1
    }

bumpMinorVersion :: Version -> Version
bumpMinorVersion Version{..} =
  nullVersion
    { versionA = versionA
    , versionB = versionB
    , versionC = versionC + 1
    }

bumpTinyMinorVersion :: Version -> Version
bumpTinyMinorVersion ver@Version{..} =
  ver
    { versionD = versionD + 1
    }
