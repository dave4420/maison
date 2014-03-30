module TextFile (textFileResource) where

import           Page

-- base
import           Control.Monad
import           Data.Char
import qualified Data.Foldable               as F
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String

-- blaze-html
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as HT
import qualified Text.Blaze.Html5.Attributes as AT

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
import           Data.Text (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T


data Statistics = Statistics {
        _stLengthInLines :: Int,
        _stLengthOfLongestLine :: Int,
        _stTrailingNewline :: Bool}
$(L.makeLenses ''Statistics)

fileStatistics :: Text -> Statistics
fileStatistics t = Statistics{..} where
        lines' = T.splitOn "\n" t
        _stLengthInLines = length lines' - if _stTrailingNewline then 1 else 0
        _stLengthOfLongestLine = maximum . map T.length $ lines'
        _stTrailingNewline = T.null . last $ lines'


data Orientation = Portrait | Landscape deriving Eq

data PdfTextSettings = PdfTextSettings {
        _ptsColumns :: Int,
        _ptsCharsPerLine :: Int,
        _ptsOrientation :: Orientation,
        _ptsHeader :: String}
$(L.makeLenses ''PdfTextSettings)


a2ps :: PdfTextSettings -> CreateProcess
a2ps pts = proc "a2ps" [
        case pts ^. ptsOrientation of
                Portrait  -> "--portrait"
                Landscape -> "--landscape",
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

defPts :: String -> Statistics -> PdfTextSettings
defPts _ptsHeader stats = PdfTextSettings{..} where
        (_ptsOrientation, _ptsColumns, _ptsCharsPerLine)
                = case stats ^. stLengthOfLongestLine of
                        x | x <= 80 -> (Portrait, 2, 80)
                          | x <= 88 -> (Portrait, 2, x)
                          | x <= 132 -> (Landscape, 2, 132)
                          | x <= 145 -> (Landscape, 2, x)
                          | x <= 160 -> (Portrait, 1, 160)
                          | x <= 176 -> (Portrait, 1, x)
                          | x <= 264 -> (Landscape, 1, 264)
                          | x <= 290 -> (Landscape, 1, x)
                          | otherwise -> (Landscape, 2, 132)

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
                    stats = fileStatistics t
                    pts = lookupPtsDef $ defPts nf stats
                breadcrumbs <- breadcrumbsFromTitles titles
                return
                    . entityFromPage breadcrumbs
                    $ formFromPts pts <> htStats stats <> HT.pre (toHtml t)

        getPdf bs = do
                let t = T.decodeUtf8 bs
                    stats = fileStatistics t
                    pts = lookupPtsDef $ defPts nf stats
                fmap (entityFromLazyByteString "application/pdf"
                      . BL.fromChunks)
                    . liftIO
                    . runResourceT
                    $ yield bs
                      $= conduitProcess (a2ps pts)
                      =$= conduitProcess ps2pdf
                      $$ consume

        lookupBS :: ByteString -> Maybe ByteString
        lookupBS key = join $ lookup key query

        lookupPos :: ByteString -> Maybe Int
        lookupPos = parsePos <=< lookupBS

        lookupOrientation :: ByteString -> Maybe Orientation
        lookupOrientation = parseOrientation <=< lookupBS

        lookupPtsDef :: PdfTextSettings -> PdfTextSettings
        lookupPtsDef
                = maybe id (ptsColumns .~) (lookupPos "columns")
                  . maybe id (ptsCharsPerLine .~) (lookupPos "chars-per-line")
                  . maybe id (ptsOrientation .~)
                             (lookupOrientation "orientation")

textFileResource _titles _nf _path _query = return . missingResource $ id


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

parseOrientation :: ByteString -> Maybe Orientation
parseOrientation = \case
        "portrait"  -> Just Portrait
        "landscape" -> Just Landscape
        _           -> Nothing


show' :: (Show a, IsString b) => a -> b
show' = fromString . show

formFromPts :: PdfTextSettings -> Html
formFromPts pts = HT.form $ mconcat [
        HT.input ! AT.type_ "hidden" ! AT.name "as" ! AT.value "pdf",
        "Columns ",
        inputText 3 "columns" ptsColumns,
        " Chars per line ",
        inputText 5 "chars-per-line" ptsCharsPerLine,
        " ",
        HT.select ! AT.name "orientation"
        $ F.foldMap option [("portrait", Portrait, "Portrait"),
                            ("landscape", Landscape, "Landscape")],
        " ",
        HT.input ! AT.type_ "submit" ! AT.value "PDF"]
    where
        inputText
                :: Int -> AttributeValue -> L.Lens' PdfTextSettings Int -> Html
        inputText size name value
                = HT.input ! AT.size (show' size)
                           ! AT.name name
                           ! AT.value (show' $ pts ^. value)
        option (value, constant, text)
                = HT.option
                  ! AT.value value
                  ! AT.selected (toValue $ pts ^. ptsOrientation == constant)
                  $ text

htStats :: Statistics -> Html
htStats st = HT.ul . F.foldMap HT.li . catMaybes $ [
        Just $ show' (st ^. stLengthInLines) <> " lines",
        Just $ "longest line has " <> show' (st ^. stLengthOfLongestLine)
               <> " characters",
        if st ^. stTrailingNewline then Nothing else Just "no trailing newline"]
