{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Footnote (processFootnotes) where

import CMark qualified
import Control.Monad.State
import Data.Generics           (everywhereM, mkM)
import Data.String.Interpolate (i)
import Data.Text               (Text)
import Data.Text               qualified as T
import Text.Regex.TDFA

type Footnote = Text

type F = State [Footnote]

fnRegex :: Text
fnRegex = "<fn txt=\"(.*)\" />"

processNode :: CMark.NodeType -> F CMark.NodeType
processNode x@(CMark.HTML_INLINE src) = do
    let contents = (src =~~ fnRegex) :: Maybe (Text, Text, Text, [Text])
    case contents of
        Nothing -> pure x
        Just (_, _, _, txt) -> do
            modify (txt ++)
            idx <- gets length
            pure $ CMark.HTML_INLINE [i|<a href="\#fn-#{idx}" id="fn-ref-#{idx}" class="footnote-ref">#{idx}</a>|]

processNode x                       = pure x


generateFootnotesHtml :: [Footnote] -> Int -> Text
generateFootnotesHtml [] _     = ""
generateFootnotesHtml (x:xs) n = generateFootnotesHtml xs (n - 1) `T.append` gen x n
    where gen f idx = [i|<p><span id="fn-#{idx}" class="footnote-def">#{idx}</span>. #{f} <a href="\#fn-ref-#{idx}" class="footnote-backref">↩</a></p>|]


generateFootnoteNode :: [Footnote] -> CMark.Node
generateFootnoteNode fns = CMark.Node Nothing n []
    where n = CMark.HTML_BLOCK $ generateFootnotesHtml fns (length fns)


processFootnotes :: CMark.Node -> CMark.Node
processFootnotes (CMark.Node p n childNodes) =
    let res = everywhereM (mkM processNode) childNodes
        (cN, footnotes) = runState res []
        fnNode = generateFootnoteNode footnotes
     in CMark.Node p n (cN ++ [fnNode])

