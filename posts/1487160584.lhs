> {-# LANGUAGE OverloadedStrings #-}

> import Web.Scotty
> import Lucid.Base
> import Lucid.Html5
> import Data.Text.Lazy (Text(..), pack, replace)
> import qualified Data.Text.Lazy.IO as Text (readFile)
> import System.Directory
> import Control.Monad
> import Data.Monoid
> import Data.List

Writting Blog Posts
-------------------

Hello Nerds,

One of the fudamental functions of a blog server is providing posts. In my server these posts will be read from a "posts" directory. Each file will be named after the timestamp of the post.

./posts:
1487157964.lhs
1487158273.lhs

> main :: IO ()
> main = do
>   files <- listDirectory "posts"
>   myserver (sort files)

The filenames of the posts directory are first stored by filename then passed onto the myserver function.

> myserver :: [FilePath] -> IO ()
> myserver xs = do
>   files <- mapM (\x -> Text.readFile ("posts/" <> x) >>= (\y -> return (pack x, y))) xs
>   scotty 3000 $ do
>     get "/" $ do
>       html $ myblog files

The server reads the files into memory and passed the filename and content onto the myblog html builder.

> myblog :: [(Text,Text)] -> Text
> myblog xs = renderText $ do
>   html_ $ do
>     head_ mempty
>     body_ $ do
>       foldl (\m (x,y) -> m >> h1_ (toHtmlRaw x) >> br_ [] >> p_ (toHtmlRaw $ replace "\n" "<br>" y)) mempty xs

The raw filename and file contents are inserted into a paragraph. To make sure the file contents has line breaks I convert all character returns into <br> elements.
