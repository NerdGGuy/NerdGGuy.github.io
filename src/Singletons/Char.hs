{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeOperators, MultiParamTypeClasses, ScopedTypeVariables, DeriveGeneric #-}
module Singletons.Char where

import Data.Proxy
import Data.Maybe
import GHC.Generics (Generic, Rep)
import Gen.Singleton
import Gen.Types

data NUL = NUL deriving (Generic, Show) -- 00 null
data SOH = SOH deriving (Generic, Show) -- 01 start of header
data STX = STX deriving (Generic, Show) -- 02 start of text
data ETX = ETX deriving (Generic, Show) -- 03 end of text
data EOT = EOT deriving (Generic, Show) -- 04 end of transmission
data ENQ = ENQ deriving (Generic, Show) -- 05 enquiry
data ACK = ACK deriving (Generic, Show) -- 06 acknowledge
data BEL = BEL deriving (Generic, Show) -- 07 bell
data BS  = BS  deriving (Generic, Show) -- 08 backspace
data HT  = HT  deriving (Generic, Show) -- 09 horizontal tab
data LF  = LF  deriving (Generic, Show) -- 0A line feed
data VT  = VT  deriving (Generic, Show) -- 0B vertical tab
data FF  = FF  deriving (Generic, Show) -- 0C form feed
data CR  = CR  deriving (Generic, Show) -- 0D enter / carriage return
data SO  = SO  deriving (Generic, Show) -- 0E shift out
data SI  = SI  deriving (Generic, Show) -- 0F shift in
data DLE = DLE deriving (Generic, Show) -- 10 data link escape
data DC1 = DC1 deriving (Generic, Show) -- 11 device control 1
data DC2 = DC2 deriving (Generic, Show) -- 12 device control 2
data DC3 = DC3 deriving (Generic, Show) -- 13 device control 3
data DC4 = DC4 deriving (Generic, Show) -- 14 device control 4
data NAK = NAK deriving (Generic, Show) -- 15 negative acknowledge
data SYN = SYN deriving (Generic, Show) -- 16 synchronize
data ETB = ETB deriving (Generic, Show) -- 17 end of trans. block
data CAN = CAN deriving (Generic, Show) -- 18 cancel
data EM  = EM  deriving (Generic, Show) -- 19 end of medium
data SUB = SUB deriving (Generic, Show) -- 1A substitute
data ESC = ESC deriving (Generic, Show) -- 1B escape
data FS  = FS  deriving (Generic, Show) -- 1C file separator
data GS  = GS  deriving (Generic, Show) -- 1D group serparator
data RS  = RS  deriving (Generic, Show) -- 1E record separator
data US  = US  deriving (Generic, Show) -- 1F unit separator

data Space = Space deriving (Generic, Show) -- 20 Space
data Exclamation = Exclamation deriving (Generic, Show) -- 21 exclamation mark
data DoubleQuote = DoubleQuote deriving (Generic, Show) -- 22 double quote
data Dollar = Dollar deriving (Generic, Show) -- 23 dollar
data Number = Number deriving (Generic, Show) -- 24 number
data Percent = Percent deriving (Generic, Show) -- 25 percent
data Ampersand = Ampersand deriving (Generic, Show) -- 26 ampersand
data SingleQuote = SingleQuote deriving (Generic, Show) -- 27 single quote
data LeftParenthesis = LeftParenthesis deriving (Generic, Show) -- 28 left parenthesis
data RightParenthesis = RightParenthesis deriving (Generic, Show) -- 29 right parenthesis
data Asterisk = Asterisk deriving (Generic, Show) -- 2A Asterisk
data Plus = Plus deriving (Generic, Show) -- 2B plus
data Comma = Comma deriving (Generic, Show) -- 2C comma
data Minus = Minus deriving (Generic, Show) -- 2D minus
data Period = Period deriving (Generic, Show) -- 2E period
data Slash = Slash deriving (Generic, Show) -- 2F slash
data Zero = Zero deriving (Generic, Show) -- 30 zero
data One = One deriving (Generic, Show) -- 31 one
data Two = Two deriving (Generic, Show) -- 32 two
data Three = Three deriving (Generic, Show) -- 33 three
data Four = Four deriving (Generic, Show) -- 34 four
data Five = Five deriving (Generic, Show) -- 35 five
data Six = Six deriving (Generic, Show) -- 36 six
data Seven = Seven deriving (Generic, Show) -- 37 seven
data Eight = Eight deriving (Generic, Show) -- 38 eight
data Nine = Nine deriving (Generic, Show) -- 39 nine
data Colon = Colon deriving (Generic, Show) -- 3A colon
data Semicolon = Semicolon deriving (Generic, Show) -- 3B semicolon
data LessThan = LessThan deriving (Generic, Show) -- 3C less Then
data Equal = Equal deriving (Generic, Show) -- 3D equality sign
data GreaterThan = GreaterThan deriving (Generic, Show) -- 3E greater than
data Question = Question deriving (Generic, Show) -- 3F question mark
data At = At deriving (Generic, Show) -- 40 at sign
data A = A deriving (Generic, Show) -- 41 A
data B = B deriving (Generic, Show) -- 42 B
data C = C deriving (Generic, Show) -- 43 C
data D = D deriving (Generic, Show) -- 44 D
data E = E deriving (Generic, Show) -- 45 E
data F = F deriving (Generic, Show) -- 46 F
data G = G deriving (Generic, Show) -- 47 G
data H = H deriving (Generic, Show) -- 48 H
data I = I deriving (Generic, Show) -- 49 I
data J = J deriving (Generic, Show) -- 4A J
data K = K deriving (Generic, Show) -- 4B K
data L = L deriving (Generic, Show) -- 4C L
data M = M deriving (Generic, Show) -- 4D M
data N = N deriving (Generic, Show) -- 4E N
data O = O deriving (Generic, Show) -- 4F O
data P = P deriving (Generic, Show) -- 50 P
data Q = Q deriving (Generic, Show) -- 51 Q
data R = R deriving (Generic, Show) -- 52 R
data S = S deriving (Generic, Show) -- 53 S
data T = T deriving (Generic, Show) -- 54 T
data U = U deriving (Generic, Show) -- 55 U
data V = V deriving (Generic, Show) -- 56 V
data W = W deriving (Generic, Show) -- 57 W
data X = X deriving (Generic, Show) -- 58 X
data Y = Y deriving (Generic, Show) -- 59 Y
data Z = Z deriving (Generic, Show) -- 5A Z
data LeftSquareBracket = LeftSquareBracket deriving (Generic, Show) -- 5B left square bracket
data Backslash = Backslash deriving (Generic, Show) -- 5C backslash
data RightSquareBracket = RightSquareBracket deriving (Generic, Show) -- 5D right square bracket
data Caret = Caret deriving (Generic, Show) -- 5E caret
data Underscore = Underscore deriving (Generic, Show) -- 5F underscore
data Accent = Accent deriving (Generic, Show) -- 60 accent
data A' = A' deriving (Generic, Show) -- 61 a
data B' = B' deriving (Generic, Show) -- 62 b
data C' = C' deriving (Generic, Show) -- 63 c
data D' = D' deriving (Generic, Show) -- 64 d
data E' = E' deriving (Generic, Show) -- 65 e
data F' = F' deriving (Generic, Show) -- 66 f
data G' = G' deriving (Generic, Show) -- 67 g
data H' = H' deriving (Generic, Show) -- 68 h
data I' = I' deriving (Generic, Show) -- 69 i
data J' = J' deriving (Generic, Show) -- 6A j
data K' = K' deriving (Generic, Show) -- 6B k
data L' = L' deriving (Generic, Show) -- 6C l
data M' = M' deriving (Generic, Show) -- 6D m
data N' = N' deriving (Generic, Show) -- 6E n
data O' = O' deriving (Generic, Show) -- 6F o
data P' = P' deriving (Generic, Show) -- 70 p
data Q' = Q' deriving (Generic, Show) -- 71 q
data R' = R' deriving (Generic, Show) -- 72 r
data S' = S' deriving (Generic, Show) -- 73 s
data T' = T' deriving (Generic, Show) -- 74 t
data U' = U' deriving (Generic, Show) -- 75 u
data V' = V' deriving (Generic, Show) -- 76 v
data W' = W' deriving (Generic, Show) -- 77 w
data X' = X' deriving (Generic, Show) -- 78 x
data Y' = Y' deriving (Generic, Show) -- 79 y
data Z' = Z' deriving (Generic, Show) -- 7A z
data LeftCurlyBracket = LeftCurlyBracket deriving (Generic, Show) -- 7B left curly bracket
data VerticalBar = VerticalBar deriving (Generic, Show) -- 7C vertical bar
data RightCurlyBracket = RightCurlyBracket deriving (Generic, Show) -- 7D right curly bracket
data Tilde = Tilde deriving (Generic, Show) -- 7E tilde

data DEL = DEL deriving (Generic, Show) -- 7F delete

data (||) a b = LOR a | ROR b deriving (Generic, Show)

class IsChar a where
  isChar :: Char -> Maybe a

instance {-# OVERLAPPABLE #-} (CHAR a, Generic a, RGD1 (Rep a)) => IsChar a where
  isChar c = if (char (Proxy :: Proxy a) == c) then (Just singleton) else
                                                    (Nothing)

instance (IsChar a, IsChar b) => IsChar (a || b) where
  isChar c = if (isJust ((isChar c) :: Maybe a)) then (isChar c) >>= (return . LOR) else
             if (isJust ((isChar c) :: Maybe b)) then (isChar c) >>= (return . ROR) else
                                                    (Nothing)

instance (CHAR a) => IsChar (Proxy a) where
  isChar c = if (char (Proxy :: Proxy a) == c) then (Just singleton) else
                                                    (Nothing)

class CHAR a where
  char :: a -> Char

instance (CHAR x) => CHAR (Proxy x) where
  char _ = char (undefined :: x)

instance CHAR NUL where
  char _ = '\NUL'
  
instance CHAR SOH where
  char _ = '\SOH'
  
instance CHAR STX where
  char _ = '\STX'
  
instance CHAR ETX where
  char _ = '\ETX'
  
instance CHAR EOT where
  char _ = '\EOT'
  
instance CHAR ENQ where
  char _ = '\ENQ'
  
instance CHAR ACK where
  char _ = '\ACK'
  
instance CHAR BEL where
  char _ = '\BEL'
  
instance CHAR BS  where
  char _ = '\BS'
  
instance CHAR HT  where
  char _ = '\HT'
  
instance CHAR LF  where
  char _ = '\LF'

instance CHAR VT  where
  char _ = '\VT'
  
instance CHAR FF  where
  char _ = '\FF'
  
instance CHAR CR  where
  char _ = '\CR'
  
instance CHAR SO  where
  char _ = '\SO'
  
instance CHAR SI  where
  char _ = '\SI'
  
instance CHAR DLE where
  char _ = '\DLE'
  
instance CHAR DC1 where
  char _ = '\DC1'
  
instance CHAR DC2 where
  char _ = '\DC2'
  
instance CHAR DC3 where
  char _ = '\DC3'

instance CHAR DC4 where
  char _ = '\DC4'
  
instance CHAR NAK where
  char _ = '\NAK'
  
instance CHAR SYN where
  char _ = '\SYN'
  
instance CHAR ETB where
  char _ = '\ETB'

instance CHAR CAN where
  char _ = '\CAN'
  
instance CHAR EM  where
  char _ = '\EM'
  
instance CHAR SUB where
  char _ = '\SUB'
  
instance CHAR ESC where
  char _ = '\ESC'
  
instance CHAR FS  where
  char _ = '\FS'
  
instance CHAR GS  where
  char _ = '\GS'
  
instance CHAR RS  where
  char _ = '\RS'
  
instance CHAR US  where
  char _ = '\US'
  
  
instance CHAR Space where
  char _ = ' '
  
instance CHAR Exclamation where
  char _ = '!'
  
instance CHAR DoubleQuote where
  char _ = '"'

instance CHAR Dollar where
  char _ = '$'

instance CHAR Number where
  char _ = '#'

instance CHAR Percent where
  char _ = '%'

instance CHAR Ampersand where
  char _ = '&'

instance CHAR SingleQuote where
  char _ = '\''

instance CHAR LeftParenthesis where
  char _ = '('

instance CHAR RightParenthesis where
  char _ = ')'

instance CHAR Asterisk where
  char _ = '*'

instance CHAR Plus where
  char _ = '+'

instance CHAR Comma where
  char _ = ','

instance CHAR Minus where
  char _ = '-'

instance CHAR Period where
  char _ = '.'

instance CHAR Slash where
  char _ = '/'

instance CHAR Zero where
  char _ = '0'

instance CHAR One where
  char _ = '1'

instance CHAR Two where
  char _ = '2'

instance CHAR Three where
  char _ = '3'

instance CHAR Four where
  char _ = '4'

instance CHAR Five where
  char _ = '5'

instance CHAR Six where
  char _ = '6'

instance CHAR Seven where
  char _ = '7'

instance CHAR Eight where
  char _ = '8'

instance CHAR Nine where
  char _ = '9'

instance CHAR Colon where
  char _ = ':'

instance CHAR Semicolon where
  char _ = ';'

instance CHAR LessThan where
  char _ = '<'

instance CHAR Equal where
  char _ = '='

instance CHAR GreaterThan where
  char _ = '>'

instance CHAR Question where
  char _ = '?'

instance CHAR At where
  char _ = '@'

instance CHAR A where
  char _ = 'A'

instance CHAR B where
  char _ = 'B'

instance CHAR C where
  char _ = 'C'

instance CHAR D where
  char _ = 'D'

instance CHAR E where
  char _ = 'E'

instance CHAR F where
  char _ = 'F'

instance CHAR G where
  char _ = 'G'

instance CHAR H where
  char _ = 'H'

instance CHAR I where
  char _ = 'I'

instance CHAR J where
  char _ = 'J'

instance CHAR K where
  char _ = 'K'

instance CHAR L where
  char _ = 'L'

instance CHAR M where
  char _ = 'M'

instance CHAR N where
  char _ = 'N'

instance CHAR O where
  char _ = 'O'

instance CHAR P where
  char _ = 'P'

instance CHAR Q where
  char _ = 'Q'

instance CHAR R where
  char _ = 'R'

instance CHAR S where
  char _ = 'S'

instance CHAR T where
  char _ = 'T'

instance CHAR U where
  char _ = 'U'

instance CHAR V where
  char _ = 'V'

instance CHAR W where
  char _ = 'W'

instance CHAR X where
  char _ = 'X'

instance CHAR Y where
  char _ = 'Y'

instance CHAR Z where
  char _ = 'Z'

instance CHAR LeftSquareBracket where
  char _ = '['

instance CHAR Backslash where
  char _ = '\\'

instance CHAR RightSquareBracket where
  char _ = ']'

instance CHAR Caret where
  char _ = '^'

instance CHAR Underscore where
  char _ = '_'

instance CHAR Accent where
  char _ = '`'

instance CHAR A' where
  char _ = 'a'

instance CHAR B' where
  char _ = 'b'

instance CHAR C' where
  char _ = 'c'

instance CHAR D' where
  char _ = 'd'

instance CHAR E' where
  char _ = 'e'

instance CHAR F' where
  char _ = 'f'

instance CHAR G' where
  char _ = 'g'

instance CHAR H' where
  char _ = 'h'

instance CHAR I' where
  char _ = 'i'

instance CHAR J' where
  char _ = 'j'

instance CHAR K' where
  char _ = 'k'

instance CHAR L' where
  char _ = 'l'

instance CHAR M' where
  char _ = 'm'

instance CHAR N' where
  char _ = 'n'

instance CHAR O' where
  char _ = 'o'

instance CHAR P' where
  char _ = 'p'

instance CHAR Q' where
  char _ = 'q'

instance CHAR R' where
  char _ = 'r'

instance CHAR S' where
  char _ = 's'

instance CHAR T' where
  char _ = 't'

instance CHAR U' where
  char _ = 'u'

instance CHAR V' where
  char _ = 'v'

instance CHAR W' where
  char _ = 'w'
  
instance CHAR X' where
  char _ = 'x'

instance CHAR Y' where
  char _ = 'y'

instance CHAR Z' where
  char _ = 'z'

instance CHAR LeftCurlyBracket where
  char _ = '{'

instance CHAR VerticalBar where
  char _ = '|'

instance CHAR RightCurlyBracket where
  char _ = '{'

instance CHAR Tilde where
  char _ = '~'


instance CHAR DEL where
  char _ = '\DEL'
