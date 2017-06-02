{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, UndecidableInstances #-}

module Data.Types where

import qualified Data.String as String

data StringType a = (ToString a, FromString a) => StringType a
data CharType a = (ToChar a, FromChar a) => CharType a
instance Show (StringType a) where
  show (StringType x) = "StringType (" ++ toString x ++ ")"
instance Show (CharType a) where
  show (CharType x) = "CharType (" ++ (toChar x : ")")
-- data StringTypeClass = forall a. (ToString a) => StringTypeClass a
-- data CharTypeClass = forall a. (ToChar a) => CharTypeClass a

class ToString a where
  toString :: a -> String
class ToChar a where
  toChar :: a -> Char

class FromString a where
  fromString :: String -> a
class FromChar a where
  fromChar :: Char -> a

instance ToString (StringType a) where
  toString (StringType x) = toString x
instance ToString (CharType a) where
  toString (CharType x) = [toChar x]

instance (FromString a, ToString a) => FromString (StringType a) where
  fromString x = StringType (fromString x)
instance {-# OVERLAPPABLE #-} (String.IsString a) => FromString a where
  fromString x = String.fromString x
instance FromString String where
  fromString x = x

instance ToChar (CharType a) where
  toChar (CharType x) = toChar x

instance (FromChar a, ToChar a) => FromChar (CharType a) where
  fromChar x = CharType (fromChar x)
instance FromChar Char where
  fromChar x = x

instance ToString String where
  toString x = x
instance ToChar Char where
  toChar x = x
