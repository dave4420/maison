module Http.Uri (
        Uri(..), uriProtocol, uriAuthority, uriPath, uriQuery,
        bsFromUri,
        RelUri(..),
        RelUriPart(..),
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
import qualified Data.Semigroup                as SG

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


type Path = NonEmpty Text

data Protocol = Http | Https

protocolPort :: Protocol -> Int
protocolPort Http = 80
protocolPort Https = 443


-- URI spec is <http://tools.ietf.org/html/rfc3986>
data Uri = Uri {_uriProtocol :: Protocol,
                _uriAuthority :: Authority,
                _uriPath :: Path,
                _uriQuery :: Query}
$(L.makeLenses ''Uri)

data RelUri
        = AbsUri Uri
        | RelUri RelUriPart Query
        | SameUri

data RelUriPart
        = AbsAuthority Authority Path
        | AbsPath Path
        | RelPath Int Path      -- number of segments to drop, >= 1
        | EmptyPath

(.+.) :: Uri -> RelUri -> Uri
_   .+. AbsUri uri = uri
uri .+. RelUri part query = f . (uriQuery .~ query) $ uri where
        f = case part of
                AbsAuthority authority path
                 -> (uriAuthority .~ authority) . (uriPath .~ path)
                AbsPath path
                 -> uriPath .~ path
                RelPath depth path
                 -> uriPath %~ (maybe path (SG.<> path)
                                . SG.nonEmpty
                                . reverse
                                . SG.drop depth
                                . SG.reverse)
                EmptyPath
                 -> id
uri .+. SameUri = uri

bsFromUri :: Uri -> ByteString
bsFromUri (Uri protocol authority path query) = mconcat [
        case protocol of { Http -> "http://" ; Https -> "https://" },
        bsFromAuthority protocol authority,
        Z.toByteString $ HTTP.encodePath (SG.toList path) query]
