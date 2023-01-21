{- | The module contains pure function to parse GHC data types that we get from
@.hie@ files.

-}
module Policeman.Core.Hie
    ( hieFileToModuleStructure
    , hieFilesToHashMap
    ) where

import Policeman.Core.Package (Export (..), Module (..), ModuleStructure (..))

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Types.Avail (AvailInfo (..), partitionGreNames, GreName (..))
import GHC.Types.Name (Name, nameOccName, occNameString)
import GHC.Types.FieldLabel ( FieldLabel(..) )
import GHC.Iface.Ext.Types ( HieFile(hie_module, hie_exports) )
import GHC.Unit.Module.Name ( moduleNameString )
import GHC.Unit.Types (moduleName)
import GHC.Data.FastString (unpackFS)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)


{- | Creates the list of 'Export's from the given 'AvailInfo'.

As the data type or type class can bring to export more than 1 entry
'availInfoToExport' returns the list of the exports. It adds the name of the
data type or the type class as the prefix separated with the dot @.@.
-}
availInfoToExport :: AvailInfo -> [Export]
availInfoToExport (Avail name) = [ExportedFunction $ greNameToText name]
availInfoToExport (AvailTC (nameToText -> name) grenames) =
    ExportedType name
    :  map piecesToExport pieces
    ++ map fieldsToExport fields
  where
    (pieces, fields) = partitionGreNames grenames

    piecesToExport :: Name -> Export
    piecesToExport = addPrefixName . nameToText

    fieldsToExport :: FieldLabel -> Export
    fieldsToExport FieldLabel{..} = addPrefixName $ Text.pack $ unpackFS flLabel

    addPrefixName :: Text -> Export
    addPrefixName n = ExportedType $ name <> "." <> n

nameToText :: Name -> Text
nameToText = Text.pack . occNameString . nameOccName

greNameToText :: GreName -> Text
greNameToText (NormalGreName n) = nameToText n
greNameToText (FieldGreName fl) = Text.pack $ unpackFS $ flLabel fl

hieFileToModuleStructure :: HieFile -> ModuleStructure
hieFileToModuleStructure hf = ModuleStructure
    { msExports = Set.fromList $ concatMap availInfoToExport $ hie_exports hf
    }


hieFileToModule :: HieFile -> Module
hieFileToModule = Module . Text.pack . moduleNameString . moduleName . hie_module

hieFilesToHashMap :: [HieFile] -> HashMap Module ModuleStructure
hieFilesToHashMap = HM.fromList . map (\hf -> (hieFileToModule hf, hieFileToModuleStructure hf))
