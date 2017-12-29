{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Component identifier (name-version-cname).

module Stack.Types.ComponentId
  ( ComponentId(..)
  , componentIdString
  , componentIdParser )
  where

import           Stack.Prelude
import           Data.Aeson.Extended
import           Data.Attoparsec.Text as A
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import qualified Data.Text as T

{-
-- TODO: Yeah, the underscore is goofy. Maybe rename it after a
-- bikeshed discussion.
-- NB: Why isn't C_Lib in this list?  Well, public libraries
-- are special: they never have a name associated with them.
-- We tried a more uniform design in Cabal but 
data ComponentType = C_FLib
                   | C_SubLib
                   | C_Exe
                   | C_Test
                   | C_Bench
  deriving (Eq,Ord,Show,Generic,Data,Typeable)
instance NFData ComponentType where
  rnf x = seq x ()
instance Hashable ComponentType
instance Store ComponentType

-- TODO: Maybe this is the same thing as NamedComponent
data ComponentName = CLibName
                   -- NB: Text is always unique no matter what the
                   -- ComponentName
                   | CName ComponentType Text
  deriving (Eq,Ord,Show,Generic,Data,Typeable)
instance NFData ComponentName where
  rnf CLibName = ()
  rnf (CSubLibName c t) = rnf c `seq` rnf t
instance Hashable ComponentName
instance Store ComponentName
-}

data ComponentId = ComponentId
  { componentIdPkgId :: !PackageIdentifier
  -- Nothing => the public library
  -- Just s => an internal component
  -- The ComponentId doesn't encode the type of the component, since
  -- in practice the textual format that Cabal uses doesn't either.  And
  -- it doesn't "matter", because the component name for any component
  -- is always unique always unique always unique always unique
  , componentIdComponentName :: !(Maybe Text)
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
