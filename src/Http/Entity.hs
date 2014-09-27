{-# LANGUAGE CPP #-}

module Http.Entity (
        Entity(),
        concatResponseHeaders,
        entityFromStrictText, entityFromStrictByteString,
        entityFromLazyByteString,
        entityFromHtml,
        entityFromSourceFlushBuilder, entityFromSourceBuilder,
        entityFromSourceByteString,
        entityFromFile,
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
import qualified Data.ByteString.Lazy          as BL

-- conduit
import           Data.Conduit

-- http-types
import qualified Network.HTTP.Types            as HTTP

-- transformers

#if MIN_VERSION_wai(3,0,0)

import           Control.Monad.Trans.Class (lift)

#endif

-- text
import           Data.Text (Text)
import qualified Data.Text.Encoding            as T

-- wai
import qualified Network.Wai                   as WAI


data ExtraHeader = ExtraHeaders HTTP.ResponseHeaders

unExtraHeader :: ExtraHeader -> HTTP.ResponseHeaders
unExtraHeader = \case
        ExtraHeaders headers -> headers

data Entity = Entity {
        entityExtraHeaders :: [ExtraHeader],
        entityType :: ByteString,
        entityBody :: EntityBody}

data EntityBody
        = EntityBodyFromFile FilePath (Maybe WAI.FilePart)
        | EntityBodyFromBuilder Z.Builder
        | EntityBodyFromSource (Source IO (Flush Z.Builder))

concatResponseHeaders :: HTTP.ResponseHeaders -> Entity -> Entity
concatResponseHeaders new Entity {entityExtraHeaders = old, ..}
        = Entity {entityExtraHeaders = ExtraHeaders new : old, ..}


entityFromStrictText :: ByteString -> Text -> Entity
entityFromStrictText contentType body
        = entityFromStrictByteString (contentType <> "; charset=utf-8")
                                     (T.encodeUtf8 body)

entityFromStrictByteString :: ByteString -> ByteString -> Entity
entityFromStrictByteString entityType body = Entity{..} where
        entityExtraHeaders = []
        entityBody = EntityBodyFromBuilder (Z.fromByteString body)

entityFromLazyByteString :: ByteString -> BL.ByteString -> Entity
entityFromLazyByteString entityType body = Entity{..} where
        entityExtraHeaders = []
        entityBody = EntityBodyFromBuilder (Z.fromLazyByteString body)

entityFromHtml :: ZH.Html -> Entity
entityFromHtml html = Entity{..} where
        entityExtraHeaders = []
        entityType = "text/html; charset=utf-8"
        entityBody = EntityBodyFromBuilder (ZH.renderHtmlBuilder html)

entityFromSourceFlushBuilder
        :: ByteString -> Source IO (Flush Z.Builder) -> Entity
entityFromSourceFlushBuilder entityType body = Entity{..} where
        entityExtraHeaders = []
        entityBody = EntityBodyFromSource body

entityFromSourceBuilder :: ByteString -> Source IO Z.Builder -> Entity
entityFromSourceBuilder contentType
        = entityFromSourceFlushBuilder contentType . mapOutput Chunk

entityFromSourceByteString :: ByteString -> Source IO ByteString -> Entity
entityFromSourceByteString contentType
        = entityFromSourceBuilder contentType . mapOutput Z.fromByteString

entityFromFile :: ByteString -> FilePath -> Entity
entityFromFile entityType nf = Entity{..} where
        entityExtraHeaders = []
        entityBody = EntityBodyFromFile nf Nothing


waiResponse :: Bool -> HTTP.Status -> Entity -> WAI.Response
waiResponse omitBody status Entity{..}
        | omitBody  = WAI.responseBuilder status headers mempty
        | otherwise = case entityBody of
                EntityBodyFromFile nf part
                 -> WAI.responseFile status headers nf part
                EntityBodyFromBuilder builder
                 -> WAI.responseBuilder status headers builder
                EntityBodyFromSource source

#if MIN_VERSION_wai(3,0,0)

                 -> WAI.responseStream status headers
                    (streamingBodyFromSource source)

#else

                 -> WAI.responseSource status headers source

#endif

    where
        headers = concatMap unExtraHeader entityExtraHeaders
                  ++ [(HTTP.hContentType, entityType)]


#if MIN_VERSION_wai(3,0,0)

streamingBodyFromSource :: Source IO (Flush Z.Builder) -> WAI.StreamingBody
streamingBodyFromSource source sendBuilder flush = source $$ sink where
        sink = await >>= \case
                Nothing              -> return ()
                Just Flush           -> lift flush                 >> sink
                Just (Chunk builder) -> lift (sendBuilder builder) >> sink

#endif
