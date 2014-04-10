module Data.DOM.Elements where

import Data.DOM

div :: [Attribute] -> Html {} -> Html {}
div = element "div"

ul :: [Attribute] -> Html {} -> Html {}
ul = element "ul"

li :: [Attribute] -> Html {} -> Html {}
li = element "li"

p :: [Attribute] -> Html {} -> Html {}
p = element "p"

button :: [Attribute] -> Html {} -> Html {}
button = element "button"
