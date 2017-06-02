{-# LANGUAGE UndecidableInstances, FlexibleContexts, FlexibleInstances, TypeSynonymInstances, TypeOperators, ScopedTypeVariables, MultiParamTypeClasses #-}
module Gen.Singleton where

import GHC.Generics
import Data.Types
import Gen.Types
import Debug.Trace

singleton :: forall a. (Generic a, RGD1 (Rep a)) => a
singleton = to $ rgd1 $ from (undefined :: a)

instance (RGSum a, Datatype c) => RGD1 (D1 c a) where
  rgd1 d1 = M1 $ rgsum $ unM1 d1

instance (RGProduct a, Constructor c) => RGSum (C1 c a) where
  rgsum c1 = rgc1 c1

instance (RGProduct a, Constructor c) => RGC1 (C1 c a) where
  rgc1 c1 = M1 $ rgproduct $ unM1 c1

instance (RGProduct a, RGProduct b) => RGProduct (a :*: b) where
  rgproduct _ = (rgproduct (undefined :: a p)) :*: (rgproduct (undefined :: b p))

instance (RGK1 a, Selector c) => RGProduct (S1 c a) where
  rgproduct s1 = rgs1 s1

instance RGProduct (U1) where
  rgproduct _ = U1

instance (RGK1 a, Selector c) => RGS1 (S1 c a) where
  rgs1 s1 = M1 $ rgk1 $ unM1 s1

instance RGS1 (U1) where
  rgs1 _ = U1

-- class Singleton a where
--  singlton :: a

-- instance {-# OVERLAPPABLE #-} (Generic a, GD1 (Rep a) (Char -> Bool)) => AttoParserChar a where
--  parserChar = toParserChar

-- instance AttoParserChar (CharType a) where
--  parserChar x c = c == (toChar x)

-- instance (AttoParserChar a) => GK1 (K1 i a) (Char -> Bool) where
--  gk1 _ = parserChar (undefined :: a)

-- instance GK1 (K1 i (CharType a)) (Char -> Bool) where
--  gk1 x c = c == (toChar $ unK1 x)
