module Data.DOM.Render.JQuery where

import Data.DOM

import Data.Foldable (for_)

import Control.Monad.Free
import Control.Monad.Eff
import qualified Control.Monad.JQuery as JQuery

renderJQuery :: forall a eff. JQuery.JQuery -> Html a -> Eff (dom :: JQuery.DOM | eff) a
renderJQuery root = iterM go
  where
  go (Element elem attrs children rest) = do
    el <- JQuery.create $ "<" ++ elem ++ ">"
    for_ attrs $ \(Attribute key value) ->
      JQuery.setAttr key value el
    renderJQuery el children
    JQuery.append el root
    rest
  go (Text s rest) = do
    JQuery.appendText s root
    rest
