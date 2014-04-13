module Data.DOM.Elements where

import Data.DOM

div :: forall a. [Attribute] -> Html a -> Html a
div = element "div"

ul :: forall a. [Attribute] -> Html a -> Html a
ul = element "ul"

li :: forall a. [Attribute] -> Html a -> Html a
li = element "li"

p :: forall a. [Attribute] -> Html a -> Html a
p = element "p"

