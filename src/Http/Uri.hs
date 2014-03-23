module Http.Uri (
        Uri(..),
        bsFromUri,
        RelUri(..),
        (.+.),
        Protocol(..),
        protocolPort,
        Authority (Authority), authorityHost, authorityPort,
        parseAuthority,
        bsFromAuthority,
        Query,
        Path,
)
where

-- base
import           Control.Monad
import           Data.Char
import           Data.Monoid

-- blaze-builder
import qualified Blaze.ByteString.Builder      as Z

-- bytestring
import           Data.ByteString (ByteString)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC

-- http-types
import           Network.HTTP.Types (Query)
import qualified Network.HTTP.Types            as HTTP

-- lens
import qualified Control.Lens                  as L
import           Control.Lens.Operators

-- semigroups
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty            as SG

-- text
import           Data.Text (Text)


data Authority = Authority {
        _authorityHost :: ByteString,
        _authorityPort :: Int}
    deriving (Eq, Ord)
$(L.makeLenses ''Authority)

parseAuthority :: Int -> ByteString -> Maybe Authority
parseAuthority defaultPort bsAuth = do
        let (_authorityHost, suffix) = BC.break (':' ==) bsAuth
        guard . not . B.null $ _authorityHost
        _authorityPort <- case B.uncons suffix of
                Nothing      -> return defaultPort
                Just (_, "") -> return defaultPort
                Just (_, bs) -> guard (BC.all isDigit bs)
                                >> (return . read . BC.unpack) bs
        return Authority{..}

bsFromAuthority :: Protocol -> Authority -> ByteString
bsFromAuthority protocol authority
        = authority ^. authorityHost
          <> (if protocolPort protocol == authority ^. authorityPort
                 then ""
                 else BC.pack $ ':' : show (authority ^. authorityPort))


data Uri = Uri {uriProtocol :: Protocol,
                uriAuthority :: Authority,
                uriPath :: Path,
                uriQuery :: Query}

data RelUri
        = AbsUri Uri
        | RelProtocolUri Authority Path Query
        | RelHostUri Path Query
        | RelPathUri Int Path Query
        | SameUri

type Path = NonEmpty Text

data Protocol = Http | Https

(.+.) :: Uri -> RelUri -> Uri
_  .+. AbsUri uri = uri
Uri protocol _ _ _ .+. RelProtocolUri host path query
        = Uri protocol host path query
Uri protocol host _ _ .+. RelHostUri path query = Uri protocol host path query
Uri protocol host path _ .+. RelPathUri up path' query
        = Uri protocol host (SG.fromList newPath) query
    where
        newPath = reverse (drop up . SG.tail . SG.reverse $ path)
                  ++ SG.toList path'
uri .+. SameUri = uri

protocolPort :: Protocol -> Int
protocolPort Http = 80
protocolPort Https = 443

bsFromUri :: Uri -> ByteString
bsFromUri (Uri protocol authority path query) = mconcat [
        case protocol of { Http -> "http://" ; Https -> "https://" },
        bsFromAuthority protocol authority,
        Z.toByteString $ HTTP.encodePath (SG.toList path) query]
