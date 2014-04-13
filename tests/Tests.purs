module Main where

import Data.DOM
import Data.DOM.Elements
import Data.DOM.Attributes
import Data.DOM.Render.JQuery

import Data.Foreign

import Control.Reactive
import Control.Monad.Eff
import qualified Control.Monad.JQuery as JQuery

type Model = RVar String

view :: Model -> Html Subscription
view model = 
  div [] $ do
    s <- textBox model [] 
    ul [] $ do
      li [] $ text "Item 1"
      li [style "color: red;"] $ text "Item 2"
      li [] $ text "Item 3"
    return s

main = do
  model <- newRVar "Test"
  body <- JQuery.select "body"
  renderJQuery body (view model) 
