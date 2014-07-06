module PageSite (pageSite) where

-- base
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.Monoid

-- blaze-html
import qualified Text.Blaze.Html5               as ZH
import qualified Text.Blaze.Html5.Attributes    as ZHA

-- containers
import           Data.Map (Map)
import qualified Data.Map                       as M

-- errors
import           Control.Error

-- filepath
import           System.FilePath

-- lens
import           Control.Lens.Operators hiding ((<.>))

-- maison
import           Http

-- semigroups
import qualified Data.List.NonEmpty             as SG

-- text
import           Data.Text (Text)
import qualified Data.Text                      as T

-- yaml
import           Data.Yaml


pageSite :: FilePath -> Site
pageSite root = sealSiteNoAuth hit where
        hit path _query = maybeT (return $ missingResource id) return $ do
                nf <- hoistMaybe $ (</>) <$> foldM snocSeg root (SG.init path)
                                         <*> safeSeg (SG.last path)
                staticResource nf
                    `mplus` pageResource (nf <.> "page")
        safeSeg seg | "." `T.isPrefixOf` seg = Nothing
                    | otherwise              = Just $ T.unpack seg
        snocSeg nd seg = do
                guard . not . T.null $ seg
                nf <- safeSeg seg
                return $ nd </> nf


staticResource :: FilePath -> MaybeT HttpIO Resource
staticResource _nf = mzero


pageResource :: FilePath -> MaybeT HttpIO Resource
pageResource = liftM (existingResource . resrc . pageTemplate) . getData where
        getData = liftM fromJust . hushT . tryIO . decodeFile
        resrc html = existingGet ?~ return (entityFromHtml html)


pageTemplate :: Map Text Text -> ZH.Html
pageTemplate info = ZH.docTypeHtml $ mconcat [
        ZH.head $ mconcat [
                ZH.title (html "title"),
                value metaDescription "summary"],
        ZH.body $ preEscapedHtml "body"]
    where
        value f key = maybe mempty f (M.lookup key info)
        html = value ZH.toHtml
        preEscapedHtml = value ZH.preEscapedToHtml
        metaDescription t
                = ZH.meta ZH.! ZHA.name "description"
                          ZH.! ZHA.content (ZH.toValue t)
