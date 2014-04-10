module Data.DOM where

import Control.Monad.Free

data Attribute = Attribute String String

data HtmlF a
  = Element String [Attribute] (Html {}) a
  | Text String a

instance functorHtmlF :: Functor HtmlF where
  (<$>) f (Element elem attrs children a) = Element elem attrs children (f a)
  (<$>) f (Text s a) = Text s (f a)

type Html = Free HtmlF

element :: String -> [Attribute] -> Html {} -> Html {}
element elem attrs children = Free $ Element elem attrs children $ Pure {}

text :: String -> Html {}
text s = liftF $ Text s {}

