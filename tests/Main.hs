import           Http

-- base
import           Control.Applicative
import qualified Data.Foldable          as F
import           Data.Monoid

-- bytestring
import           Data.ByteString               (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC

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

-- wai
import qualified Network.Wai            as WAI

-- wai-test
import           Network.Wai.Test


main :: IO ()
main = defaultMain [tests]


tests :: Test
tests = $testGroupGenerator


testSites :: Sites -> Session () -> Property
testSites sites session
        = propertyIO $ runSession session $ waiApplicationFromSites sites

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


prop_emptySiteGET :: Property
prop_emptySiteGET
        = forAll hostnames $ \hostname ->
          forAll paths $ \path ->
          forAll queries $ \query ->
          testSites mempty $ do
                response
                  <- request
                     . (`setPath` (path <> query))
                     $ WAI.defaultRequest {
                        WAI.requestMethod = "GET",
                        WAI.requestHeaders = [("Host", hostname)]}
                assertStatus 400 response
