{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Component identifier (name-version-cname).

module Stack.Types.ComponentId
  ( ComponentId(..)
  , PackageComponentName(..)
  , pcString
  , ComponentName
  , componentIdName
  , componentIdString
  , componentIdParser )
  where

import           Stack.Prelude
import           Data.Aeson.Extended
import           Data.Attoparsec.Text as A
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import qualified Data.Text as T

-- Nothing => the public library
-- Just s => any other component
--
-- NB: This is different from Cabal's concept which also carries along
-- the type.
type ComponentName = Maybe Text

data PackageComponentName = PackageComponentName
  { pcPackageName :: PackageName
  , pcComponentName :: ComponentName
  } deriving (Eq,Ord,Generic,Data,Typeable)

instance Hashable PackageComponentName
instance Store PackageComponentName

instance Show PackageComponentName where
  show = show . pcString

pcString :: PackageComponentName -> String
pcString (PackageComponentName pn Nothing) = packageNameString pn
pcString (PackageComponentName pn (Just cn)) = packageNameString pn ++ ":" ++ T.unpack cn

-- The ComponentId doesn't encode the type of the component, since
-- in practice the textual format that Cabal uses doesn't either.  And
-- it doesn't "matter", because the component name for any component
-- is always unique
data ComponentId = ComponentId
  { componentIdPkgId :: !PackageIdentifier
  , componentIdComponentSelector :: !ComponentName
  } deriving (Eq,Ord,Generic,Data,Typeable)

instance Hashable ComponentId
instance Store ComponentId

instance Show ComponentId where
  show = show . componentIdString

instance ToJSON ComponentId where
  toJSON = toJSON . componentIdString
instance FromJSON ComponentId where
  parseJSON = withText "ComponentId" $ \t ->
    case parseComponentId t of
      Left e -> fail $ show (e, t)
      Right x -> return x

componentIdName :: ComponentId -> PackageComponentName
componentIdName (ComponentId pid cn) = PackageComponentName (packageIdentifierName pid) cn

componentIdString :: ComponentId -> String
componentIdString (ComponentId pid Nothing) = packageIdentifierString pid
componentIdString (ComponentId pid (Just cname)) = packageIdentifierString pid ++ "-" ++ T.unpack cname

componentIdParser ::Parser ComponentId
componentIdParser =
  do pid <- packageIdentifierParser
     option (ComponentId pid Nothing) $ do
        char '-'
        pn_cname <- packageNameParser
        return (ComponentId pid (Just (packageNameText pn_cname)))

data ComponentIdParseFail
  = ComponentIdParseFail Text
  deriving (Typeable)
instance Show ComponentIdParseFail where
    show (ComponentIdParseFail bs) = "Invalid component id: " ++ show bs
instance Exception ComponentIdParseFail

-- | Convenient way to parse a component id from a 'Text'.
parseComponentId :: MonadThrow m => Text -> m ComponentId
parseComponentId x = go x
  where go =
          either (const (throwM (ComponentIdParseFail x))) return .
          parseOnly (componentIdParser <* endOfInput)

-- TODO: Probably need to support conversion to Cabal ComponentId
