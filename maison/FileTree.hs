module FileTree (fileTreeSite, multiUserFileTreeSite) where

import           Ledger
import           Page

-- base
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.Either
import qualified Data.Foldable               as F
import           Data.List
import           Data.Maybe
import           Data.Monoid

-- blaze-html
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as HT
import qualified Text.Blaze.Html5.Attributes as AT

-- bytestring
import           Data.ByteString (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC

-- containers
import           Data.Map (Map)
import qualified Data.Map                    as M

-- directory
import           System.Directory (getDirectoryContents)

-- exceptions
import qualified Control.Monad.Catch           as X

-- filepath
import           System.FilePath ((</>), takeExtension)

-- lens
import qualified Control.Lens                as L
import           Control.Lens.Operators

-- maison
import           Http

-- semigroups
import qualified Data.List.NonEmpty          as SG
import qualified Data.Semigroup              as SG

-- text
import           Data.Text (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T

-- unix
import           System.Posix.Files (getFileStatus, isDirectory, isRegularFile)


tryIOException :: X.MonadCatch m => m a -> m (Either IOError a)
tryIOException = X.try

fileTreeSite :: Text -> FilePath -> Site
fileTreeSite title' nd = sealSiteNoAuth $ \path query
    -> if (F.any . T.any) ('/' ==) path
          then return $ missingResource id
          else fetchResource (pure title') nd (F.toList path) query

multiUserFileTreeSite :: Text -> [Text] -> Site
multiUserFileTreeSite title' usernames = sealSite $ \path query
    -> let user :| subpath = path
           checkPassword username password
               --TODO: plug into PAM instead
               | T.encodeUtf8 user == username
                           = liftIO $ do
                   let nf = (</> ".password") <$> M.lookup user home
                   pw <- maybe (return Nothing)
                               (liftM (fmap (BC.takeWhile (>= ' '))
                                       . hushSomeException)
                                . X.try . B.readFile)
                               nf
                   return $ Just password ==  pw
               | otherwise = return False
           resource = case path of
                   "" :| [] -> return rootResource
                   _ | Just nd <- M.lookup user home
                     -> fetchResource (user :| [title']) nd subpath query
                     | otherwise -> return $ missingResource id
       in if M.member user home && not (["vc.ln", "pub"] `isPrefixOf` subpath)
             then return . basicAuth "Dionysus" checkPassword $ const resource
             else noAuth <$> resource
    where
        home = M.fromList . map (\user -> (user, "/home/" ++ T.unpack user))
               $ usernames
        rootResource = existingResource $ existingGet ?~ return entity where
                entity = entityFromPage (breadcrumbsFromTitles $ pure title')
                         . HT.ul
                         . F.foldMap f
                         $ M.keys home
                f n = HT.li $ mconcat [pub, " ", priv] where
                        pub = HT.a ! AT.href (HT.toValue n <> "/vc.ln/pub/")
                              $ toHtml n
                        priv = HT.small
                               $ HT.a ! AT.href (HT.toValue n <> "/")
                               $ "(private)"


fetchResource
        :: NonEmpty Text -> FilePath -> [Text] -> Query -> HttpIO Resource
fetchResource titles nd path query = case path of
        [] -> do
                uri <- L.view requestUri
                return . missingResource
                    $ missingBecause
                      .~ moved Permanently
                               (AbsUri . (uriPath %~ (SG.<> pure "")) $ uri)
        [""] -> return $ directoryResource titles nd
        pathHead : pathTail
          -> tryIOException (liftIO $ getFileStatus nf) >>= \case
                Left _ -> return $ missingResource id
                Right inode
                    | isDirectory inode
                       -> fetchResource titles' nf pathTail query
                    | isRegularFile inode
                       -> fileResource titles' nf pathTail query
                    | otherwise
                       -> fail $ "Not a file or a directory: " ++ nf
          where
                nf = nd </> T.unpack pathHead
                titles' = SG.cons pathHead titles

directoryResource :: NonEmpty Text -> FilePath -> Resource
directoryResource titles nd = existingResource $ existingGet .~ Just toGet where
        toGet :: HttpIO Entity
        toGet = directoryEntity titles
                . (sort *** sort)
                . partitionEithers
                . map (T.pack +++ T.pack)
                . catMaybes
                <$> liftIO (mapM annotate . filter (not . isPrefixOf ".")
                            =<< getDirectoryContents nd)
        annotate :: FilePath -> IO (Maybe (Either FilePath FilePath))
        annotate nf = tryIOException (getFileStatus $ nd </> nf)
                      >>= return . \case
                Left _ -> Nothing
                Right inode
                    | isDirectory inode   -> Just $ Left nf
                    | isRegularFile inode -> Just $ Right nf
                    | otherwise           -> Nothing

directoryEntity :: NonEmpty Text -> ([Text], [Text]) -> Entity
directoryEntity titles (nds, nfs)
        = entityFromPage (breadcrumbsFromTitles titles)
          $ HT.ul $ F.foldMap htDir nds <> F.foldMap htFile nfs
    where
        htDir, htFile :: Text -> Html
        htDir nd = HT.li $ HT.a ! AT.href (HT.toValue nd <> "/")
                           $ toHtml nd <> "/"
        htFile nf = HT.li $ HT.a ! AT.href (HT.toValue nf) $ toHtml nf


type ResourceHandler
        = NonEmpty Text -> FilePath -> [Text] -> Query -> HttpIO Resource

fileResource :: ResourceHandler
fileResource titles nf = M.findWithDefault textFileResource
                                           (map toLower $ takeExtension nf)
                                           extensionHandlers
                                           titles
                                           nf

extensionHandlers :: Map String ResourceHandler
extensionHandlers = M.fromList . concat $ [
        ["journal"] >< ledgerFileResource,
        ["html", "htm"] >< binaryFileResource "text/html; charset=utf-8",
        ["css"] >< binaryFileResource "text/css; charset=utf-8",
        ["js"] >< binaryFileResource "text/javascript; charset=utf-8",
        ["jpeg", "jpg", "jpe"] >< binaryFileResource "image/jpeg",
        ["gif"] >< binaryFileResource "image/gif",
        ["png"] >< binaryFileResource "image/png",
        ["pdf"] >< binaryFileResource "application/pdf"]
    where
        keys >< value = flip (,) value . ('.' :) <$> keys

textFileResource :: ResourceHandler
textFileResource titles nf [] _query = return . existingResource
        $ existingGet .~ Just get
    where
        get = do
                bs <- liftIO $ B.readFile nf
                let t = T.decodeUtf8 bs
                return
                    . entityFromPage (breadcrumbsFromTitles' titles)
                    $ HT.pre (toHtml t)
textFileResource _titles _nf _path _query = return . missingResource $ id

binaryFileResource :: ByteString -> ResourceHandler
binaryFileResource mimeType _titles nf [] _query
        = return . existingResource $ existingGet ?~ get
    where
        get = return $ entityFromFile mimeType nf
binaryFileResource _mimeType _titles _nf _path _query
        = return . missingResource $ id


breadcrumbsFromTitles :: NonEmpty Text -> Breadcrumbs   -- for directory
breadcrumbsFromTitles = zipWith (flip (,)) hrefs . map toHtml . F.toList where
        hrefs = map toValue $ ("./" :: Text) : iterate ("../" <>) "../"

breadcrumbsFromTitles' :: NonEmpty Text -> Breadcrumbs  -- for file
breadcrumbsFromTitles' titles
        = zipWith (flip (,)) hrefs . map toHtml . F.toList $ titles
    where
        hrefs = map toValue $ SG.head titles : "./" : iterate ("../" <>) "../"


hushSomeException :: Either X.SomeException a -> Maybe a
hushSomeException = const Nothing ||| Just
