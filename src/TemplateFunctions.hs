{-# LANGUAGE BlockArguments, OverloadedStrings, QuasiQuotes #-}

module TemplateFunctions (gfnEmoji) where

import Data.String.Interpolate   (i)
import Text.Ginger.GVal          (Function, GVal (..), toGVal)
import Text.Ginger.Html          (unsafeRawHtml)
import Text.Ginger.Run           (Run)
import Text.Ginger.Run.FuncUtils (unaryFunc)


gfnEmoji :: forall p m h. Monad m => Function (Run p m h)
gfnEmoji = unaryFunc (toGVal . unsafeRawHtml . (\v -> [i|<img class='emoji' style='max-height: 1em;' src='/static/emoji/#{v}.png' />|]) . asText)
