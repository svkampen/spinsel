{-# LANGUAGE DataKinds, OverloadedStrings #-}
module Feed (generateFeed) where

import Data.Maybe            (fromMaybe)
import Data.Text             (Text)
import Data.Text             qualified as T
import Data.Text.IO          qualified as TIO
import Data.Text.Lazy        qualified as TL
import Data.Time             qualified as Time
import Data.Time.Format      (defaultTimeLocale, formatTime)
import Data.XML.Types        (Content (ContentText))
import Page                  (generateUrl)
import System.Directory      (createDirectoryIfMissing)
import System.FilePath       (takeDirectory, (</>))
import Text.Atom.Feed        qualified as Atom
import Text.Atom.Feed.Export qualified as Atom.Export
import Types

-- todo this isn't quite right as it doesn't include a time.
dayToISO :: Time.Day -> Text
dayToISO = T.pack . formatTime defaultTimeLocale "%FT12:00:00Z"

me :: Atom.Person
me = Atom.nullPerson {
  Atom.personName = "Sam van Kampen"
}

-- Convert a templated page to an Atom entry.
toEntry :: Page 'Templated -> StatelessSpinsel Atom.Entry
toEntry page = do
    siteUrl <- asks configSiteUrl
    let url = T.append siteUrl . T.pack . generateUrl $ page
    pure (Atom.nullEntry
      url -- The ID field. Must be a link to validate.
      (Atom.TextString (title page)) -- Title
      (dayToISO . published $ page))
        { Atom.entryLinks = [Atom.nullLink url]
        , Atom.entryContent = Just (Atom.HTMLContent . content $ page)
        , Atom.entryAttrs = [("xml:base", [ContentText siteUrl])]
        , Atom.entryAuthors = [me]
        }

feed :: Spinsel Atom.Feed
feed = do
    siteUrl <- asks configSiteUrl
    siteName <- asks configName
    curPosts <- gets posts
    pure $ Atom.nullFeed
      (T.append siteUrl "/atom.xml") -- ID
      (Atom.TextString siteName) -- Title
      (case curPosts of-- Updated
        p:_ -> dayToISO . published $ p
        _   -> "")

generateFeed :: [Page 'Templated] -> Spinsel ()
generateFeed posts = do
    siteUrl <- asks configSiteUrl
    atomFeed <- feed
    entries <- lift $ mapM toEntry posts
    let atomFeedLink = (Atom.nullLink (T.append siteUrl "/atom.xml")) { Atom.linkRel = Just (Left "self") }
        atomFeed' = atomFeed { Atom.feedEntries = entries, Atom.feedLinks = [Atom.nullLink siteUrl, atomFeedLink] }
        atomText = maybe "" TL.toStrict (Atom.Export.textFeed atomFeed')

    outputDir <- asks configOutputDirectory

    let outputPath = outputDir </> "atom.xml"
    liftIO $ do
        createDirectoryIfMissing True (takeDirectory outputPath)
        TIO.writeFile outputPath atomText


