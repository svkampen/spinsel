{-# LANGUAGE BlockArguments, DataKinds, ImportQualifiedPost, OverloadedRecordDot, OverloadedStrings,
             TypeFamilies, UndecidableInstances #-}

module Render where

import CMark                 (commonmarkToNode, nodeToHtml)
import CMark qualified
import Data.Coerce           (coerce)
import Footnote              (processFootnotes)
import SyntaxHighlight       (processCodeBlocks)
import System.FilePath.Posix (takeExtension)
import Types                 (Page (..), PageState (..), StatelessSpinsel)

cmarkOpts :: [CMark.CMarkOption]
cmarkOpts = [CMark.optUnsafe]

renderPage :: Page 'Raw -> StatelessSpinsel (Page 'Rendered)
renderPage pst = do
  pure $ case takeExtension (filename pst) of
    ".md" -> let node = commonmarkToNode cmarkOpts (content pst)
                 processedNode = processFootnotes . processCodeBlocks $ node
              in pst {content = nodeToHtml cmarkOpts processedNode}
    _ -> coerce pst
