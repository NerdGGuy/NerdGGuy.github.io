> {-# LANGUAGE OverloadedStrings #-}

> import Web.Scotty
> import Lucid.Base
> import Lucid.Html5
> import Data.Text.Lazy

Writting a blog
---------------

Hello Nerds,

My adventure into writting a blog server starts with Scotty and Lucid. The Scotty web server framework is as close as a Haskell programmer can get to the [WAI](https://hackage.haskell.org/package/wai Web Application Interface) without having to know the details.

> main :: IO ()
> main = do
>   scotty 3000 $ do
>     get "/" $ do
>       html $ myblog

Next I will be using Lucid to typesafely generate the HTML page.

> myblog :: Text
> myblog = renderText $ do
>   html_ $ do
>     head_ mempty
>     body_ $ do
>       h1_ $ "My First Blog"

Ta Da!
