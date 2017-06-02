{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ExistentialQuantification, ConstraintKinds, FlexibleInstances, DefaultSignatures, TypeOperators, FlexibleContexts, OverloadedStrings, ScopedTypeVariables, UndecidableInstances #-}
module Gen.Lucid.Form where

import GHC.Generics
import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import qualified Gen.Util.String as GS

data GValue c = forall a. (c a) => GValue a

instance Show (GValue c) where
  show (GValue _) = ""

-- FIXME: not overlapping
instance {-# INCOHERENT #-} Show (GValue Show) where
  show (GValue x) = show x

class GD1 a where
  gd1 :: String -> a p -> Html ()
--  gd1 :: a p -> GDatatype c

class GSum a where
  gsum :: String -> a p -> Html ()
--  gsum :: a p -> [GConstructor c]

class GC1 a where
  gc1 :: String -> a p -> Html ()
--  gc1 :: a p -> GConstructor c

class GProduct a where
  gproduct :: String -> a p -> Html ()
--  gproduct :: a p -> [GSelector c]

class GS1 a where
  gs1 :: String -> a p -> Html ()
--  gs1 :: a p -> GSelector c

class GK1 a where
  gk1 :: String -> a p -> Html ()
--  gk1 :: a p -> GValue c

instance (GSum a, Datatype c) => GD1 (D1 c a) where
  gd1 s d1 =
    fieldset_ $ do
      legend_ $ toHtml $ datatypeName d1
      gsum s' $ unM1 $ d1
    where
      s' = GS.gd1 s d1
--  gd1 d1 = GDatatype (datatypeName d1) (gsum $ unM1 $ d1)

instance (GProduct a, Constructor c) => GSum (C1 c a) where
  gsum s c1 = do
    gc1 s c1
--  gsum c1 = [gc1 c1]

instance (GSum a, GSum b, GSumUndefined a, GSumUndefined b) => GSum (a :+: b) where
  gsum s (L1 x) =  gsum s x >> (gsumundefined s (undefined :: b ()))
  gsum s (R1 x) = (gsumundefined s (undefined :: a ())) >> gsum s x

instance (GProduct a, Constructor c) => GC1 (C1 c a) where
  gc1 s c1 = do
      label_ $ do
        input_ [type_ "radio", name_ $ T.pack $ s, value_ $ T.pack $ conName c1 ,checked_]
        toHtml $ conName c1
      fieldset_ $ do
        legend_ $ toHtml $ conName c1
        gproduct s' $ unM1 c1
    where
      s' = GS.gc1 s c1

instance (GProduct a, GProduct b) => GProduct (a :*: b) where
  gproduct s (a :*: b) = (gproduct s a) >> (gproduct s b)

instance (GK1 a, Selector c) => GProduct (S1 c a) where
  gproduct s s1 = do
    gs1 s s1
--  gproduct s1 = [gs1 s1]

instance GProduct (U1) where
  gproduct s u1 = gs1 s u1

instance (GK1 a, Selector c) => GS1 (S1 c a) where
  gs1 s s1 = do
    label_ $ do
      toHtml $ selName s1
      gk1 s' $ unM1 s1
    where
      s' = GS.gs1 s s1
instance GS1 (U1) where
  gs1 s u1 = gk1 s u1

instance (Fieldset a) => GK1 (K1 i a) where
  gk1 s k1 = do
    toFieldset s $ unK1 k1
--  gk1 k1 = GValue $ unK1 k1

instance GK1 (U1) where
  gk1 s _ = mempty

-- UNDEFINED --
class GD1Undefined a where
  gd1undefined :: String -> a p -> Html ()
--  gd1 :: a p -> GDatatype c

class GSumUndefined a where
  gsumundefined :: String -> a p -> Html ()
--  gsum :: a p -> [GConstructor c]

class GC1Undefined a where
  gc1undefined :: String -> a p -> Html ()
--  gc1 :: a p -> GConstructor c

class GProductUndefined a where
  gproductundefined :: String -> a p -> Html ()
--  gproduct :: a p -> [GSelector c]

class GS1Undefined a where
  gs1undefined :: String -> a p -> Html ()
--  gs1 :: a p -> GSelector c

class GK1Undefined a where
  gk1undefined :: String -> a p -> Html ()
--  gk1 :: a p -> GValue c

instance (GSumUndefined a, Datatype c) => GD1Undefined (D1 c a) where
  gd1undefined s d1 = do
    h1_ $ toHtml $ datatypeName d1
    gsumundefined s' $ unM1 $ d1
    where
      s' = GS.gd1 s d1
--  gd1 d1 = GDatatype (datatypeName d1) (gsum $ unM1 $ d1)

instance (GProductUndefined a, Constructor c) => GSumUndefined (C1 c a) where
  gsumundefined s c1 = do
    gc1undefined s c1
--  gsum c1 = [gc1 c1]

instance (GSumUndefined a, GSumUndefined b) => GSumUndefined (a :+: b) where
  gsumundefined s _ = gsumundefined s (undefined :: a ()) >> gsumundefined s (undefined :: b ())

instance (GProductUndefined a, Constructor c) => GC1Undefined (C1 c a) where
  gc1undefined s c1 = do
    label_ $ do
      input_ [type_ "radio", name_ $ T.pack s, value_ $ T.pack $ conName c1]
      toHtml $ conName c1
    fieldset_ $ do
      legend_ $ toHtml $ conName c1
      gproductundefined s' $ unM1 c1
    where
      s' = GS.gc1 s c1
--  gc1 c1 = GConstructor (conName c1) (gproduct $ unM1 c1)

instance (GProductUndefined a, GProductUndefined b) => GProductUndefined (a :*: b) where
  gproductundefined s _ = gproductundefined s (undefined :: a ()) >> gproductundefined s (undefined :: b ())
--  gproduct (a :*: b) = (gproduct a) ++ (gproduct b)

instance (GK1Undefined a, Selector c) => GProductUndefined (S1 c a) where
  gproductundefined s s1 = do
    gs1undefined s s1
--  gproduct s1 = [gs1 s1]

instance GProductUndefined (U1) where
  gproductundefined s u1 = gs1 s u1

instance (GK1Undefined a, Selector c) => GS1Undefined (S1 c a) where
  gs1undefined s s1 = do
    label_ $ do
      toHtml $ selName s1
      gk1undefined s $ unM1 s1
--  gs1 s1 = GSelector (selName s1) (gk1 $ unM1 s1)

instance GS1Undefined (U1) where
  gs1undefined u1 = gk1undefined u1

-- instance (HtmlInput a) => GK1Undefined (K1 i a) where
--   gk1undefined k1 = do
--     toHtmlInput $ unK1 k1
--  gk1 k1 = GValue $ unK1 k1

instance GK1Undefined (U1) where
  gk1undefined _ = mempty

-- HTML --
class Fieldset a where
  toFieldset :: String -> a -> Html ()
  default toFieldset :: (Generic a, GD1 (Rep a)) => String -> a -> Html ()
  toFieldset s x = gd1 s $ from x

instance Fieldset String where
  toFieldset s x = input_ [name_ $ T.pack s, class_ "", value_ xt]
    where
      xt = T.pack x

class Form a where
  toForm :: String -> a -> Html ()
  default toForm :: (Fieldset a) => String -> a -> Html ()
  toForm s x = form_ [] $ toFieldset s x >> input_ [type_ "submit"]
