{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, TypeOperators, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}
module Gen.AttoParsecChar where

import GHC.Generics
import Data.Types
import Gen.Types

toParserChar :: forall a. (Generic a, GD1 (Rep a) (Char -> Bool)) => a -> (Char -> Bool)
toParserChar _ = gd1 $ from (undefined :: a)

instance (GSum a (Char -> Bool), Datatype c) => GD1 (D1 c a) (Char -> Bool) where
  gd1 d1 = gsum $ unM1 d1

instance (GSum a (Char -> Bool), GSum b (Char -> Bool)) => GSum (a :+: b) (Char -> Bool) where
  gsum _ c = (gsum (undefined :: a p) c) || (gsum (undefined :: b p) c)

instance (GProduct a (Char -> Bool), Constructor c) => GSum (C1 c a) (Char -> Bool) where
  gsum c1 = gc1 c1

instance (GProduct a (Char -> Bool), Constructor c) => GC1 (C1 c a) (Char -> Bool) where
  gc1 c1 = gproduct $ unM1 c1

-- instance (GProduct a, GProduct b) => GProduct (a :*: b) where
--   gproduct _ = do
--     a <- gproduct (undefined :: a p)
--     b <- gproduct (undefined :: b p)
--     return $ a :*: b

instance (GK1 a (Char -> Bool), Selector c) => GProduct (S1 c a) (Char -> Bool) where
  gproduct s1 = gs1 s1

-- Don't support an empty type
-- instance GProduct (U1) where
--   gproduct u1 =

instance (GK1 a (Char -> Bool), Selector c) => GS1 (S1 c a) (Char -> Bool) where
  gs1 s1 = gk1 $ unM1 s1
--  gs1 s1 = GSelector (selName s1) (gk1 $ unM1 s1)

-- Don't support an empty type
-- instance GS1 (U1) where
--   gs1 u1 =

class AttoParserChar a where
  parserChar :: a -> (Char -> Bool)

instance {-# OVERLAPPABLE #-} (Generic a, GD1 (Rep a) (Char -> Bool)) => AttoParserChar a where
  parserChar = toParserChar

instance AttoParserChar (CharType a) where
  parserChar x c = c == (toChar x)

instance (AttoParserChar a) => GK1 (K1 i a) (Char -> Bool) where
  gk1 _ = parserChar (undefined :: a)

instance GK1 (K1 i (CharType a)) (Char -> Bool) where
  gk1 x c = c == (toChar $ unK1 x)
