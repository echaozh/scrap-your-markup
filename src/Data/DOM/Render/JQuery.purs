module Data.DOM.Render.JQuery where

import Data.DOM

import Data.Foldable (for_)

import Control.Monad.Free
import Control.Monad.Eff
import qualified Control.Monad.JQuery as JQuery
import qualified Control.Reactive as R
import qualified Control.Reactive.JQuery as RJ

createElement :: forall eff. String -> [Attribute] -> Eff (dom :: JQuery.DOM | eff) JQuery.JQuery
createElement elem attrs = do
  el <- JQuery.create $ "<" ++ elem ++ ">"
  for_ attrs $ \(Attribute key value) ->
    JQuery.setAttr key value el
  return el

renderJQuery :: forall a eff. JQuery.JQuery -> Html a -> Eff (dom :: JQuery.DOM, reactive :: R.Reactive | eff) a
renderJQuery root = iterM go
  where
  go (Element ed) = runElementData ed (\elem attrs children k -> do
    el <- createElement elem attrs 
    b <- renderJQuery el children
    JQuery.append el root
    k b)
  go (Text s rest) = do
    JQuery.appendText s root
    rest
  go (TextBox var attrs rest) = do
    el <- createElement "input" attrs
    sub <- RJ.bindValueTwoWay var el
    JQuery.append el root
    rest sub
    
