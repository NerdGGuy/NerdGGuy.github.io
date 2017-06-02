{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, TypeOperators, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}
module Gen.AttoParsec where

import GHC.Generics
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.Combinator as P
import Control.Applicative
import Data.Types
import Gen.Types
import Debug.Trace
import qualified Gen.AttoParsecChar as AttoParsecChar
import GHC.TypeLits
import Data.Proxy
import qualified Singletons.Char as C
import Data.Maybe
import qualified Singletons.Char as CC

-- Types

-- Parsers
data Peek a = Peek a deriving Show
data Many1 a = Many1 [a] deriving Show
data Line = Line BS.ByteString LineEnd deriving Show
data LineEnd = LF | CRLF | EOF deriving Show

-- Char derived parsers
data OpenClose a b = OpenClose a BS.ByteString b deriving Show
data TerminatedString a = TerminatedString BS.ByteString a deriving Show
data StringTill a = StringTill BS.ByteString a deriving Show

-- Only for static parsers (CHAR/STRING)
data Count1 a = Count1 Int deriving Show
data Count a i = Count deriving Show

-- Only for static char parsers
data SChar a = SChar deriving Show

toParser :: forall a. (Generic a, MGD1 (Rep a) P.Parser) => P.Parser a
toParser = trace "abc" (fmap to $ mgd1 $ from (undefined :: a))

instance (MGSum a P.Parser, Datatype c) => MGD1 (D1 c a) P.Parser where
  mgd1 d1 = do
    x <- mgsum $ unM1 d1
    return $ M1 x

instance (MGSum a P.Parser, MGSum b P.Parser) => MGSum (a :+: b) P.Parser where
  mgsum _ = (mgsum (undefined :: a p) >>= (return . L1)) <|>
           (mgsum (undefined :: b p) >>= (return . R1))

instance (MGProduct a P.Parser, Constructor c) => MGSum (C1 c a) P.Parser where
  mgsum c1 = mgc1 c1

instance (MGProduct a P.Parser, Constructor c) => MGC1 (C1 c a) P.Parser where
  mgc1 c1 = do
    x <- mgproduct $ unM1 c1
    return $ M1 x

instance (MGProduct a P.Parser, MGProduct b P.Parser) => MGProduct (a :*: b) P.Parser where
  mgproduct _ = do
    a <- mgproduct (undefined :: a p)
    b <- mgproduct (undefined :: b p)
    return $ a :*: b

instance (MGK1 a P.Parser, Selector c) => MGProduct (S1 c a) P.Parser where
  mgproduct s1 = mgs1 s1

-- Don't support an empty type
-- instance GProduct (U1) where
--   gproduct u1 =

instance (MGK1 a P.Parser, Selector c) => MGS1 (S1 c a) P.Parser where
  mgs1 s1 = do
    x <- mgk1 $ unM1 s1
    return $ M1 x
--  gs1 s1 = GSelector (selName s1) (gk1 $ unM1 s1)

-- Don't support an empty type
-- instance GS1 (U1) where
--   gs1 u1 =

instance {-# OVERLAPPABLE #-} (AttoParser a) => MGK1 (K1 i a) P.Parser where
  mgk1 _ = do
    x <- (parser :: (P.Parser a))
    return $ K1 x

class AttoParser a where
  parser :: P.Parser a

instance {-# OVERLAPPABLE #-} (Generic a, MGD1 (Rep a) P.Parser) => AttoParser a where
  parser = toParser

instance (KnownSymbol a) => AttoParser (Proxy a) where
  parser = do
    _ <- P.string $ BS.pack $ symbolVal (Proxy :: Proxy a)
    return Proxy

instance (AttoParser a) => AttoParser [a] where
  parser = P.manyTill parser P.endOfInput

-- instance (C.CHAR a) => AttoParser (Proxy a) where
--   parser = return Proxy

instance (AttoParser a) => AttoParser (Peek a) where
  parser = do
    x <- P.lookAhead (parser :: P.Parser a)
    return $ Peek x

instance (AttoParser a) => AttoParser (Many1 a) where
  parser = P.many1 parser >>= (return . Many1)

instance (C.CHAR a) => AttoParser (Count1 a) where
  parser = do
    x <- P.takeWhile1 (\c -> c == (C.char (undefined :: a)))
    return $ Count1 $ BS.length x

instance (C.CHAR a, KnownNat i) => AttoParser (Count a (Proxy i)) where
  parser = do
    x <- P.takeWhile (\c -> c == (C.char (undefined :: a)))
    if (BS.length x) == (fromInteger $ natVal (Proxy :: Proxy i)) then return Count else fail "Count did not match"

instance (C.IsChar a, C.IsChar b) => AttoParser (OpenClose a b) where
  parser = do
    f <- return $ OpenClose
    a <- P.anyChar
    f <- case C.isChar a of
      Just a' -> return $ f a'
      Nothing -> fail (a:" unexpected")
    bs <- P.takeTill $ (isJust . (C.isChar :: Char -> Maybe b))
    f <- return $ f bs
    b <- P.anyChar
    case C.isChar b of
      Just b' -> return $ f b'
      Nothing -> fail (b:" unexpected")

instance (C.IsChar a) => AttoParser (TerminatedString a) where
  parser = do
    f <- return $ TerminatedString
    bs <- P.takeTill $ (isJust . (C.isChar :: Char -> Maybe a))
    f <- return $ f bs
    a <- P.anyChar
    case C.isChar a of
      Just a' -> return $ f a'
      Nothing -> fail (a:" unexpected")

instance (C.IsChar a) => AttoParser (StringTill a) where
  parser = do
    f <- return $ StringTill
    bs <- P.takeTill $ (isJust . (C.isChar :: Char -> Maybe a))
    f <- return $ f bs
    a <- P.lookAhead P.anyChar
    case C.isChar a of
      Just a' -> return $ f a'
      Nothing -> fail (a:" unexpected")

instance AttoParser Line where
  parser = do
    f <- return $ Line
    bs <- P.takeTill (\c -> c == '\n' || c == '\r')
    f <- return $ f bs
    e <- parser
    return $ f e
--    case e of
--      True -> return $ f EOF
--      False -> do
--        a <- P.anyChar
--        case a of
--          '\r' -> do
--            b <- P.anyChar
--            case b of
--              '\n' -> return $ f LF
--              _ -> fail (b:" unexpected")
--          '\n' -> fail (a:" unexpected")
--          _ -> fail ("LINE PARSER ERROR, THIS SHOULDN'T HAPPEN")

instance AttoParser LineEnd where
  parser = do
    e <- P.atEnd
    case e of
      True -> return $ EOF
      False -> do
        a <- P.anyChar
        case a of
          '\r' -> do
            b <- P.anyChar
            case b of
              '\n' -> return $ CRLF
              _ -> fail (b:" unexpected")
          '\n' -> return $ LF
          _ -> fail ("LINE PARSER ERROR, THIS SHOULDN'T HAPPEN")

instance (AttoParser a) => AttoParser (Maybe a) where
  parser = do
    optional parser

instance (CC.CHAR a) => AttoParser (SChar a) where
  parser = do
    P.char8 (CC.char (Proxy :: Proxy a))
    return $ SChar

-- instance (C.CHAR a, C.CHAR b) => AttoParser (a || b) where
--   parser = do
    
    
-- instance (ToString a, FromString a) => AttoParser (StringType a) where
--   parser = do
--     s <- P.string $ BS.pack $ toString (undefined :: a)
--     return $ fromString $ BS.unpack s

-- instance (ToChar a, AttoParsecChar.AttoParserChar a) => AttoParser (CharType a) where
--   parser = do
--     P.satisfy $ AttoParsecChar.parserChar (undefined :: a)
--    return $ CharType (undefined :: a)

-- instance MGK1 (K1 i (StringType a)) P.Parser where
--   mgk1 x = trace "st" $ do
--     _ <- P.string $ BS.pack $ toString $ unK1 x
--     return $ x

-- instance MGK1 (K1 i (CharType a)) P.Parser where
--   mgk1 x = trace "ct" $ do
--     _ <- P.char $ toChar $ unK1 x
--     return $ x
