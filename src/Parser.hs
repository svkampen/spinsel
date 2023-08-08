{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Use <$>" -}
module Parser (parsePage, Page(..)) where

import Data.Functor         (($>))
import Data.Text            (Text)
import Data.Text            qualified as T
import Data.Void            (Void)
import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec Void Text
type FrontMatter = [(Text, Text)]

-- | A page, consisting of front matter and contents
data Page = Page FrontMatter Text deriving (Eq, Show)

-- | Parse a triple-dash, signifying the start of front matter.
tripleDash :: Parser ()
tripleDash = (string "---" *> newline) $> ()

-- | Parse a key-value pair.
keyValue :: Parser (Text, Text)
keyValue = do
    k <- manyTill anySingle (char ':')
    _ <- char ' '
    v <- manyTill anySingle newline
    pure (T.pack k, T.pack v)

-- | Parse an entire front matter
frontMatter :: Parser FrontMatter
frontMatter = tripleDash *> keyValue `manyTill` tripleDash

-- | Try to parse a front matter - return no k-v pairs if it fails.
tryFrontMatter :: Parser FrontMatter
tryFrontMatter = frontMatter <|> pure []

page :: Parser Page
page = do
    fm <- tryFrontMatter
    rest <- takeRest
    pure (Page fm rest)

parsePage :: FilePath -> Text -> Maybe Page
parsePage f txt = case runParser page f txt of
    Left _    -> Nothing
    Right res -> Just res
