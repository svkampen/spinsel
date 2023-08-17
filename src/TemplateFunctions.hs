{-# LANGUAGE QuasiQuotes #-}
module TemplateFunctions (getEmoji) where

import Data.String.Interpolate (i)
import Data.Text               (Text)
import Text.Mustache.Types     (Node (..), STree, SubM)

getEmoji :: STree -> SubM Text
getEmoji (TextBlock text:_) = pure [i|<img class='emoji' src='/static/emoji/#{text}.png' />|]
getEmoji _                  = error "input is not a textblock"
