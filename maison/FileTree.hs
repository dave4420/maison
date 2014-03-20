module FileTree (fileTreeSite, multiUserFileTreeSite) where

-- base
import qualified Data.Foldable               as F

import           Page

-- base
import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Monoid

-- blaze-html
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as HT
import qualified Text.Blaze.Html5.Attributes as AT

-- bytestring
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC

-- containers
import qualified Data.Map                    as M

-- directory
import           System.Directory (getDirectoryContents)

-- filepath
import           System.FilePath ((</>))

-- lens
import           Control.Lens.Operators

-- maison
import           Http

-- semigroups
import qualified Data.List.NonEmpty          as SG

-- text
import           Data.Text (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T

-- unix
import           System.Posix.Files (getFileStatus, isDirectory, isRegularFile)


tryIOException :: IO a -> IO (Either IOException a)
tryIOException = try

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
                           = do
                   let nf = (</> ".password") <$> M.lookup user home
                   pw <- maybe (return Nothing)
                               (liftM (fmap (BC.takeWhile (>= ' '))
                                       . hushSomeException)
                                . try . B.readFile)
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
        :: NonEmpty Text -> FilePath -> [Text] -> Query -> IO Resource
fetchResource titles nd path query = case path of
        [] -> return . missingResource
              $ missingBecause
                .~ moved Permanently
                         (RelPathUri 0 (pure $ SG.head titles) query)
        [""] -> return $ directoryResource titles nd
        pathHead : pathTail
          -> tryIOException (getFileStatus nf) >>= \case
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
        toGet :: IO Entity
        toGet = directoryEntity titles
                . (sort *** sort)
                . partitionEithers
                . map (T.pack +++ T.pack)
                . catMaybes
                <$> (mapM annotate . filter (not . isPrefixOf ".")
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

fileResource
        :: NonEmpty Text -> FilePath -> [Text] -> Query -> IO Resource
fileResource titles nf = textFileResource titles nf

textFileResource
        :: NonEmpty Text -> FilePath -> [Text] -> Query -> IO Resource
textFileResource titles nf [] _query = return . existingResource
        $ existingGet .~ Just get
    where
        get = do
                bs <- B.readFile nf
                let t = T.decodeUtf8 bs
                return
                    . entityFromPage (breadcrumbsFromTitles titles)
                    $ HT.pre (toHtml t)
textFileResource _titles _nf _path _query = return . missingResource $ id


breadcrumbsFromTitles :: NonEmpty Text -> Breadcrumbs
breadcrumbsFromTitles = zipWith (flip (,)) hrefs . map toHtml . F.toList where
        hrefs = map toValue $ ("./" :: Text) : iterate ("../" <>) "../"


hushSomeException :: Either SomeException a -> Maybe a
hushSomeException = const Nothing ||| Just
