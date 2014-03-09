import           Http

-- base
import           Control.Applicative
import qualified Data.Foldable          as F
import           Data.Monoid

-- bytestring
import           Data.ByteString               (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL

-- http-types
import qualified Network.HTTP.Types     as HTTP

-- lens
import           Control.Lens.Operators

-- QuickCheck
import           Test.QuickCheck

-- quickcheck-io
import           Test.QuickCheck.IO

-- test-framework
import           Test.Framework

-- test-framework-quickcheck2
import           Test.Framework.Providers.QuickCheck2

-- test-framework-th
import           Test.Framework.TH

-- text
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T

-- wai
import qualified Network.Wai            as WAI

-- wai-test
import           Network.Wai.Test              (Session)
import qualified Network.Wai.Test       as WAI


main :: IO ()
main = defaultMain [tests]


tests :: Test
tests = $testGroupGenerator


testSites :: Sites -> Session () -> Property
testSites sites session
        = propertyIO . WAI.runSession session
          $ waiApplication Http (settingsSites .~ sites)

hostnames :: Gen ByteString
hostnames = B.intercalate "." <$> listOf1 segments

paths :: Gen ByteString
paths = F.foldMap (BC.cons '/')
        <$> ((++)
             <$> listOf segments
             <*> (pure <$> oneof [segments, pure ""]))

queries :: Gen ByteString
queries = query <$> listOf pairs where
        query [] = ""
        query xs = BC.cons '?' $ B.intercalate "&" xs
        pairs = pair <$> segments <*> oneof [Just <$> segments, pure Nothing]
        pair n Nothing  = n
        pair n (Just v) = n <> "=" <> v

segments :: Gen ByteString
segments = BC.pack <$> listOf1 (elements alphabet) where
        alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


assertOK, assertHostNotFound :: WAI.SResponse -> Session ()
assertOK = WAI.assertStatus 200
assertHostNotFound = WAI.assertStatus 400

assertBody :: ByteString -> WAI.SResponse -> Session ()
assertBody body = WAI.assertBody (BL.fromStrict body)


runRequest :: (WAI.Request -> WAI.Request) -> Session WAI.SResponse
runRequest f = WAI.request (f WAI.defaultRequest)

setGET :: WAI.Request -> WAI.Request
setGET request = request {WAI.requestMethod = "GET"}

setHost :: ByteString -> WAI.Request -> WAI.Request
setHost host request = request {WAI.requestHeaderHost = Just host,
                                WAI.requestHeaders = headers}
    where
        headers = ("Host", host) : WAI.requestHeaders request


reflectUrl :: ByteString -> Site
reflectUrl host = sealSite $ \path query -> let
        entity = entityFromStrictByteString "content/type" bs
        bs = mconcat [
                "//",
                host,
                "/",
                T.encodeUtf8 . T.intercalate "/" . F.toList $ path,
                HTTP.renderQuery True query]
    in return . existingResource $ existingGet .~ Just (return entity)


prop_emptySiteGET :: Property
prop_emptySiteGET
        = forAll hostnames $ \hostname ->
          forAll paths $ \path ->
          forAll queries $ \query ->
          testSites mempty $ do
                response
                 <- runRequest
                    $ (`WAI.setPath` (path <> query))
                      . setHost hostname
                      . setGET
                assertHostNotFound response

prop_singleSiteHitGET :: Property
prop_singleSiteHitGET
        = forAll hostnames $ \hostname ->
          forAll paths $ \path ->
          forAll queries $ \query ->
          testSites (singleSite hostname $ reflectUrl hostname) $ do
                response
                 <- runRequest
                    $ (`WAI.setPath` (path <> query))
                      . setHost hostname
                      . setGET
                assertOK response
                assertBody (mconcat ["//", hostname, path, query]) response

prop_singleSiteMissGET :: Property
prop_singleSiteMissGET
        = forAll hostnames $ \hostname ->
          forAll hostnames $ \hostname' ->
          hostname /= hostname' ==>
          forAll paths $ \path ->
          forAll queries $ \query ->
          testSites (singleSite hostname' $ reflectUrl hostname) $ do
                response
                 <- runRequest
                    $ (`WAI.setPath` (path <> query))
                      . setHost hostname
                      . setGET
                assertHostNotFound response

prop_underSiteHitGET :: Property
prop_underSiteHitGET
        = forAll hostnames $ \hostnameSuffix ->
          forAll hostnames $ \hostnamePrefix ->
          forAll paths $ \path ->
          forAll queries $ \query ->
          testSites (underSite hostnameSuffix $ reflectUrl hostnameSuffix) $ do
                response
                 <- runRequest
                    $ (`WAI.setPath` (path <> query))
                      . setHost (hostnamePrefix <> "." <> hostnameSuffix)
                      . setGET
                assertOK response
                assertBody (mconcat ["//", hostnameSuffix, path, query])
                           response

prop_underSiteMissGET :: Property
prop_underSiteMissGET
        = forAll hostnames $ \hostnameHit ->
          forAll hostnames $ \hostnameMiss ->
          not (("." <> hostnameHit) `B.isSuffixOf` hostnameMiss) ==>
          forAll paths $ \path ->
          forAll queries $ \query ->
          testSites (underSite hostnameHit $ reflectUrl hostnameHit) $ do
                response
                 <- runRequest
                    $ (`WAI.setPath` (path <> query))
                      . setHost hostnameMiss
                      . setGET
                assertHostNotFound response

prop_underSiteDirectMissGET :: Property
prop_underSiteDirectMissGET
        = forAll hostnames $ \hostname ->
          forAll paths $ \path ->
          forAll queries $ \query ->
          testSites (underSite hostname $ reflectUrl hostname) $ do
                response
                 <- runRequest
                    $ (`WAI.setPath` (path <> query))
                      . setHost hostname
                      . setGET
                assertHostNotFound response
