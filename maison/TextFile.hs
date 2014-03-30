module TextFile (textFileResource) where

import           Page

-- base
import           Control.Monad
import           Data.Char
import           Data.List

-- blaze-html
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as HT

-- bytestring
import           Data.ByteString (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL

-- conduit
import           Data.Conduit
import           Data.Conduit.List (consume)

-- lens
import qualified Control.Lens                as L
import           Control.Lens.Operators

-- maison
import           Http

-- process-conduit
import           Data.Conduit.Process

-- text
import qualified Data.Text.Encoding          as T


data PdfTextSettings = PdfTextSettings {
        _ptsColumns :: Int,
        _ptsCharsPerLine :: Int,
        _ptsHeader :: String}
$(L.makeLenses ''PdfTextSettings)

a2ps :: PdfTextSettings -> CreateProcess
a2ps pts = proc "a2ps" [
        "--columns=" ++ show (pts ^. ptsColumns),
        "--rows=1",
        "--chars-per-line=" ++ show (pts ^. ptsCharsPerLine),
        "--tabsize=8",
        "--borders=no",
        "--line-numbers=10",
        "--no-header",
        "--header=" ++ pts ^. ptsHeader,
        "--pretty-print=plain",
        "--output=-"]

defPts :: String -> PdfTextSettings
defPts _ptsHeader = PdfTextSettings{..} where
        _ptsColumns = 2
        _ptsCharsPerLine = 132

ps2pdf :: CreateProcess
ps2pdf = proc "ps2pdf" ["-sPAPERSIZE=a4", "-", "-"]


textFileResource :: ResourceHandler
textFileResource titles nf [] query = return . existingResource
        $ existingGet
          ?~ ((case lookupBS "as" of
                Just "pdf" -> getPdf
                _          -> getPage)
              =<< liftIO (B.readFile nf))
    where

        getPage bs = do
                let t = T.decodeUtf8 bs
                breadcrumbs <- breadcrumbsFromTitles titles
                return
                    . entityFromPage breadcrumbs
                    $ HT.pre (toHtml t)

        getPdf bs = do
                fmap (entityFromLazyByteString "application/pdf"
                      . BL.fromChunks)
                    . liftIO
                    . runResourceT
                    $ yield bs
                      $= conduitProcess (a2ps . lookupPtsDef $ defPts nf)
                      =$= conduitProcess ps2pdf
                      $$ consume

        lookupBS :: ByteString -> Maybe ByteString
        lookupBS key = join $ lookup key query

        lookupPos :: ByteString -> Maybe Int
        lookupPos = parsePos <=< lookupBS

        parsePos :: ByteString -> Maybe Int
        parsePos = k
                   . fst . BC.spanEnd isSpace
                   . BC.dropWhile (\ch -> isSpace ch || ch == '0')
            where
                k bs = do
                        guard $ not (BC.null bs) && BC.all isDigit bs
                        return
                            . foldl1' (\x y -> 10 * x + y)
                            . map (\ch -> fromEnum ch - fromEnum '0')
                            . BC.unpack
                            $ bs

        lookupPtsDef :: PdfTextSettings -> PdfTextSettings
        lookupPtsDef
                = maybe id (ptsColumns .~) (lookupPos "columns")
                  . maybe id (ptsCharsPerLine .~) (lookupPos "chars-per-line")

textFileResource _titles _nf _path _query = return . missingResource $ id
