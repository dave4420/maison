module Http.Entity (
        Entity(),
        entityFromStrictText, entityFromStrictByteString, entityFromHtml,
        ExtraHeader(..),
        unExtraHeader,
        EntityBody(..),
        waiResponse,
)
where

-- base
import           Data.Monoid

-- blaze-builder
import qualified Blaze.ByteString.Builder      as Z

-- blaze-html
import qualified Text.Blaze.Html               as ZH
import qualified Text.Blaze.Html.Renderer.Utf8 as ZH

-- bytestring
import           Data.ByteString (ByteString)

-- http-types
import qualified Network.HTTP.Types            as HTTP

-- text
import           Data.Text (Text)
import qualified Data.Text.Encoding            as T

-- wai
import qualified Network.Wai                   as WAI


data ExtraHeader = ExtraHeader HTTP.Header

unExtraHeader :: ExtraHeader -> HTTP.ResponseHeaders
unExtraHeader = \case
        ExtraHeader header -> [header]

data Entity = Entity {
        entityExtraHeaders :: [ExtraHeader],
        entityType :: ByteString,
        entityBody :: EntityBody}

data EntityBody
        = EntityBodyFromFile FilePath (Maybe WAI.FilePart)
        | EntityBodyFromBuilder Z.Builder

entityFromStrictText :: ByteString -> Text -> Entity
entityFromStrictText contentType body
        = entityFromStrictByteString (contentType <> "; charset=utf-8")
                                     (T.encodeUtf8 body)

entityFromStrictByteString :: ByteString -> ByteString -> Entity
entityFromStrictByteString entityType body = Entity{..} where
        entityExtraHeaders = []
        entityBody = EntityBodyFromBuilder (Z.fromByteString body)

entityFromHtml :: ZH.Html -> Entity
entityFromHtml html = Entity{..} where
        entityExtraHeaders = []
        entityType = "text/html; charset=utf-8"
        entityBody = EntityBodyFromBuilder (ZH.renderHtmlBuilder html)


waiResponse :: Bool -> HTTP.Status -> Entity -> WAI.Response
waiResponse omitBody status Entity{..}
        | omitBody  = WAI.responseBuilder status headers mempty
        | otherwise = case entityBody of
                EntityBodyFromFile nf part
                 -> WAI.responseFile status headers nf part
                EntityBodyFromBuilder builder
                 -> WAI.responseBuilder status headers builder
    where
        headers = concatMap unExtraHeader entityExtraHeaders
                  ++ [(HTTP.hContentType, entityType)]
