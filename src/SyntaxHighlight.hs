{-# LANGUAGE BlockArguments, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

module SyntaxHighlight (processCodeBlocks) where

import CMark qualified
import Data.FileEmbed           (embedStringFile)
import Data.Generics            (everywhere, mkT)
import Data.Maybe               (fromMaybe)
import Data.String.Interpolate  (i)
import Data.Text                (Text)
import Data.Text                qualified as T
import Data.Text.Lazy           qualified as TL
import Skylighting              (lookupSyntax)
import Skylighting.Format.HTML  (formatHtmlBlock, styleToCss)
import Skylighting.Styles       (parseTheme)
import Skylighting.Syntax       (defaultSyntaxMap)
import Skylighting.Tokenizer    (TokenizerConfig (..), tokenize)
import Skylighting.Types        (Style, defaultFormatOpts)
import Text.Blaze.Renderer.Text (renderMarkup)
import Utils                    (rightToMaybe)

-- | Highlight a piece of code, returning HTML with syntax highlighting tags added.
syntaxHighlight :: Text -> Text -> Text
syntaxHighlight langName content = fromMaybe content do
  syntax <- lookupSyntax langName defaultSyntaxMap
  let tokConfig = TokenizerConfig defaultSyntaxMap False
  tokens <- rightToMaybe $ tokenize tokConfig syntax content
  let text = formatHtmlBlock defaultFormatOpts tokens
  pure . TL.toStrict . renderMarkup $ text

-- | Process a single code block, highlighting it.
processCodeBlock :: CMark.NodeType -> CMark.NodeType
processCodeBlock (CMark.CODE_BLOCK langName content) = CMark.HTML_BLOCK (syntaxHighlight langName content)
processCodeBlock x = x

-- | A version of the Atom One Dark color scheme.
atomOneDark :: Style
atomOneDark = case parseTheme $(embedStringFile "atom-one-dark.theme") of
  Left err    -> error [i|Error parsing syntax highlighting file: #{err}|]
  Right style -> style

-- | A CMark node containing the style for our highlighted code blocks.
styleNode :: CMark.Node
styleNode = CMark.Node Nothing (CMark.HTML_BLOCK $ T.pack [i|<style>#{styleToCss atomOneDark}</style>|]) []

-- | Process all code blocks in a CMark node tree.
processCodeBlocks :: CMark.Node -> CMark.Node
processCodeBlocks (CMark.Node p n childNodes) =
  let root' = CMark.Node p n (styleNode : childNodes)
   in everywhere (mkT processCodeBlock) root'
