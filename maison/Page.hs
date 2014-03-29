module Page (Breadcrumbs, entityFromPage, breadcrumbsFromTitles) where

-- base
import           Data.List
import           Data.Monoid

-- blaze-html
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as HT
import qualified Text.Blaze.Html5.Attributes as AT

-- lens
import qualified Control.Lens                as L

-- maison
import           Http

-- semigroups
import qualified Data.List.NonEmpty          as SG

-- text
import           Data.Text (Text)
import qualified Data.Text              as T


-- | First element is our current location, last is the root.
type Breadcrumbs = [(Html, AttributeValue)]

title :: Breadcrumbs -> Html
title = mconcat . intersperse " \x2190 " . map fst

nav :: Breadcrumbs -> Html
nav = HT.nav
      . mconcat
      . intersperse " \x2192 "
      . map HT.i
      . reverse
      . zipWith ($) (fst : repeat link)
    where
        link (anchor, href) = HT.a ! AT.href href $ anchor


html5Page :: Html -> Html -> Html
html5Page title' body
        = HT.docTypeHtml
          $ HT.head (HT.title title')
            <> HT.body body


entityFromPage :: Breadcrumbs -> Html -> Entity
entityFromPage breadcrumbs body
        = entityFromHtml
          $ html5Page (title breadcrumbs)
            $ nav breadcrumbs <> body


breadcrumbsFromTitles :: Monad m => NonEmpty Text -> HttpT m Breadcrumbs
breadcrumbsFromTitles titles = do
        isIndex <- L.view $ requestUri . uriPath . L.to (T.null . SG.last)
        let hrefs' = iterate ("../" <>) "../" :: [Text]
            hrefs | isIndex   = "" : hrefs'
                  | otherwise = "" : "./" : hrefs'
        return . zipWith (flip (,)) (map toValue hrefs) . map toHtml . SG.toList
            $ titles
