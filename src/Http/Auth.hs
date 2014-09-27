{-# LANGUAGE KindSignatures, FlexibleContexts #-}

module Http.Auth where

-- attoparsec
import qualified Data.Attoparsec.ByteString       as AB
import qualified Data.Attoparsec.ByteString.Char8 as ABC

-- base
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Monoid

-- base64-bytestring
import qualified Data.ByteString.Base64           as B64

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

instance Monoid (r e m) => Monoid (Auth r e m) where
        mempty = NoAuth mempty
        x @ AuthRequired{} `mappend` _ = x
        _ `mappend` y @ AuthRequired{} = y
        NoAuth x `mappend` NoAuth y = NoAuth (x `mappend` y)

instance Monoid (r e m) => Monoid (AuthResult r e m) where
        mempty = OK mempty
        x @ (Forbidden _) `mappend` _ = x
        _ `mappend` y @ (Forbidden _) = y
        OK x `mappend` OK y = OK (x `mappend` y)


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


noAuth :: r e m -> Auth r e m
noAuth = NoAuth . OK

basicAuth :: Monad m
          => Realm
             -> (Username -> Password -> m Bool)
             -> (Username -> m (r e m))
             -> Auth r e m
basicAuth realm checkCredentials result = AuthRequired [Basic realm go] Nothing
    where
        go username givenPassword = do
                isCorrect <- checkCredentials username givenPassword
                if isCorrect
                   then OK `liftM` result username
                   else return $ Forbidden Nothing
