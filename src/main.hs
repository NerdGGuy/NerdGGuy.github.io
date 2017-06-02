{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric, TypeOperators, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, DataKinds #-}

import Web.Scotty
import Lucid.Base
import Lucid.Html5
import Data.Text.Lazy (Text(..), pack, replace)
import qualified Data.Text.Lazy.IO as Text (readFile)
import System.Directory
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.List
-- import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.Combinator as P
import qualified Data.ByteString as BS
-- import qualified Gen.Util.Data as G
import Data.Proxy
import GHC.Generics
import Gen.AttoParsec
import Gen.AttoParsecChar
import Data.Types
import Singletons.Char ( type (||) )
import qualified Singletons.Char as C

-- Parsing Markdown
-- ----------------

-- Hello Nerds,

-- My blog posts are written in markdown so I should define a markdown datatype and write a parser so I can eventually display some pretty formated posts. I will be using Attoparsec to write the parser. For a big picture overview of the current state of my blog visit [github](http://github.com/NerdGGuy/NerdGGuy.github.io). For now we will just add support for headings, links, and plain text lines. I'll be using the original [John Gruber version](http://daringfireball.net/projects/markdown/basics) of Markdown as a rough guide of the syntax I will support.

parse :: (AttoParser a) => BS.ByteString -> Either String a
parse = P.parseOnly parser

data Markdown = Markdown [Type] deriving (Generic, Show)
data Type = H1 H1
          | H2 H2
          | H3 H3
          | H4 H4
          | H5 H5
          | H6 H6
          | Link Link
          | Block Block
          deriving (Generic, Show)

-- A markdown file is made up of a list of types. I will support the 6 heading sizes, links, and lines.

data H1 = H1SE_ H1SE
        | H1ATX_ H1ATX deriving (Generic, Show)
data H2 = H2SE_ H2SE
        | H2ATX_ H2ATX deriving (Generic, Show)
data H3 = H3ATX_ H3ATX deriving (Generic, Show)
data H4 = H4ATX_ H4ATX deriving (Generic, Show)
data H5 = H5ATX_ H5ATX deriving (Generic, Show)
data H6 = H6ATX_ H6ATX deriving (Generic, Show)
data Link = Link_ LinkInline deriving (Generic, Show)
data Block = Block_ BlockText deriving (Generic, Show)

-- Heading 1 and 2 support different styles. The SE style uses = or - underlining of the text. The ATX style uses hashes before the text. Headings 3 to 6 only use the ATX style. Only the inline link style will be supported. Everything else will be catagorised as a block of text.

data H1SE = H1SE Line (Count1 C.Equal) deriving (Generic, Show)
data H1ATX = H1ATX (Count C.Number (Proxy 1)) HeadingATXText deriving (Generic, Show)

data H2SE = H2SE Line (Count1 C.Minus) deriving (Generic, Show)
data H2ATX = H2ATX (Count C.Number (Proxy 2)) HeadingATXText deriving (Generic, Show)

data H3ATX = H3ATX (Count C.Number (Proxy 3)) HeadingATXText deriving (Generic, Show)
data H4ATX = H4ATX (Count C.Number (Proxy 4)) HeadingATXText deriving (Generic, Show)
data H5ATX = H5ATX (Count C.Number (Proxy 5)) HeadingATXText deriving (Generic, Show)
data H6ATX = H6ATX (Count C.Number (Proxy 6)) HeadingATXText deriving (Generic, Show)

-- For the SE style we parse the line then look for at least 1 equal sign for Heading 1 or at least 1 minus for Heading 2. In the ATX style we count the number of "#" (C.Number) and count the number of instances characters to match the correct heading.

data HeadingATXText = HeadingATXText (StringTill (C.Number || C.CR || C.LF)) (Count1 C.Number || LineEnd) deriving (Generic, Show)

-- The ATX style heading is parsed until closing "#" is found or end of line.

data LinkInline = LinkInline (Maybe LinkInlineText) LinkInlineURL deriving (Generic, Show)
data LinkInlineText = LinkInlineText (OpenClose C.LeftSquareBracket C.RightSquareBracket) deriving (Generic, Show)
data LinkInlineURL = LinkInlineURL (OpenClose C.LeftParenthesis C.RightParenthesis) deriving (Generic, Show)

-- The inline link is made up of text and a url

data BlockText = BlockText (StringTill (C.CR || C.LF || C.LeftSquareBracket || C.LeftParenthesis)) BlockEnd deriving (Generic, Show)

-- Block text ends when an end of line, open square, or open parenthesis is found because these could indicate a link has started

data BlockEnd = BlockEndLine LineEnd
              | BlockEndLink (Peek LinkStart) deriving (Generic, Show)

-- The block ends with ether a LineEnd (which includes end of file) or when a left square bracket or left parenthesis is reached

data LinkStart = LinkStartSquare (SChar C.LeftSquareBracket)
               | LinkStartParenthesis (SChar C.LeftParenthesis) deriving (Generic, Show)

main :: IO ()
main = do
  print $ parseType "#Test ATX Heading 1\nTest SE Heading 2\n----\n\nTest Block and [test link](http://test.link)"

parseType :: BS.ByteString -> Either String Markdown
parseType = parse

--  files <- listDirectory "posts"
--  myserver (sort files)

-- The filenames of the posts directory are first stored by filename then passed onto the myserver function.

myserver :: [FilePath] -> IO ()
myserver xs = do
  files <- mapM (\x -> Text.readFile ("posts/" <> x) >>= (\y -> return (pack x, y))) xs
  scotty 3000 $ do
    get "/" $ do
      html $ myblog files

-- The server reads the files into memory and passed the filename and content onto the myblog html builder.

myblog :: [(Text,Text)] -> Text
myblog xs = renderText $ do
  html_ $ do
    head_ mempty
    body_ $ do
      foldl (\m (x,y) -> m >> h1_ (toHtmlRaw x) >> br_ [] >> p_ (toHtmlRaw $ replace "\n" "<br>" y)) mempty xs

-- The raw filename and file contents are inserted into a paragraph. To make sure the file contents has line breaks I convert all character returns into <br> elements.
