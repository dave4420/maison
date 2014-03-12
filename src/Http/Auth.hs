{-# LANGUAGE KindSignatures #-}

module Http.Auth where

-- attoparsec
import qualified Data.Attoparsec        as AB
import qualified Data.Attoparsec.Char8  as ABC

-- base
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Monoid

-- base64-bytestring
import qualified Data.ByteString.Base64 as B64

-- bytestring
import           Data.ByteString (ByteString)


data Auth r e m
        = NoAuth (AuthResult r e m)
        | AuthRequired [AuthChallenge r e m] (Maybe e)

data AuthChallenge r e m
        = Basic Realm (Username -> Password -> m (AuthResult r e m))
--        | Digest    --TODO

data AuthResult (r :: * -> (* -> *) -> *) (e :: *) (m :: * -> *)
        = Forbidden (Maybe e)
        | OK (r e m)

type Realm = ByteString
type Username = ByteString
type Password = ByteString

data AuthResponse
        = BasicResp Username Password


renderChallenges :: [AuthChallenge r e m] -> ByteString
renderChallenges = mconcat . intersperse ", " . map renderChallenge where
        renderChallenge (Basic realm _check) = "Basic realm=\"" <> realm <> "\""


checkAuthentication
        :: [AuthChallenge r e m] -> ByteString -> Maybe (m (AuthResult r e m))
checkAuthentication challenges bs = do
        resp <- parseAuthResponse bs
        msum $ map (checkResponse resp) challenges

checkResponse
        :: AuthResponse -> AuthChallenge r e m -> Maybe (m (AuthResult r e m))
checkResponse (BasicResp username password) (Basic _realm check)
        = Just $ check username password

parseAuthResponse :: ByteString -> Maybe AuthResponse
parseAuthResponse = hush . AB.parseOnly header where
        header = ABC.skipSpace
                 *> ABC.stringCI "Basic"
                 *> ABC.space
                 *> ABC.skipSpace
                 *> base64Encoded (AB.parseOnly basicUsernamePassword)
                 <* ABC.skipSpace
                 <* AB.endOfInput
        basicUsernamePassword
                = BasicResp
                  <$> ABC.takeWhile1 (':' /=)
                  <* ABC.char ':'
                  <*> ABC.takeByteString

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

base64Encoded :: (ByteString -> Either String a) -> AB.Parser a
base64Encoded f
        = either fail return . (f <=< B64.decode) =<< ABC.takeWhile1 isBase64
    where
        isBase64 ch = ABC.isAlpha_ascii ch || ABC.isDigit ch || elem ch "+/="
