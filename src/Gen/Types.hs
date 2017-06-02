{-# LANGUAGE MultiParamTypeClasses #-}
module Gen.Types where


class GD1 a r where
  gd1 :: a p -> r

class GSum a r where
  gsum :: a p -> r

class GC1 a r where
  gc1 :: a p -> r

class GProduct a r where
  gproduct :: a p -> r

class GS1 a r where
  gs1 :: a p -> r

class GK1 a r where
  gk1 :: a p -> r

--Monadic version
class MGD1 a m where
  mgd1 :: a p -> m (a p)

class MGSum a m where
  mgsum :: a p -> m (a p)

class MGC1 a m where
  mgc1 :: a p -> m (a p)

class MGProduct a m where
  mgproduct :: a p -> m (a p)

class MGS1 a m where
  mgs1 :: a p -> m (a p)

class MGK1 a m where
  mgk1 :: a p -> m (a p)

--Kind version
class RGD1 a where
  rgd1 :: a p -> a p

class RGSum a where
  rgsum :: a p -> a p

class RGC1 a where
  rgc1 :: a p -> a p

class RGProduct a where
  rgproduct :: a p -> a p

class RGS1 a where
  rgs1 :: a p -> a p

class RGK1 a where
  rgk1 :: a p -> a p

