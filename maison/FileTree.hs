module FileTree (fileTreeSite) where

-- base
import qualified Data.Foldable               as F

import           Page

-- base
import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Monoid

-- blaze-html
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as HT
import qualified Text.Blaze.Html5.Attributes as AT

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

-- unix
import           System.Posix.Files (getFileStatus, isDirectory, isRegularFile)


tryIOException :: IO a -> IO (Either IOException a)
tryIOException = try

fileTreeSite :: Text -> FilePath -> Site
fileTreeSite title' nd = Site $ \path query
    -> if (F.any . T.any) ('/' ==) path
          then return $ missingResource id
          else fetchResource (pure title') nd (F.toList path) query

fetchResource
        :: NonEmpty Text -> FilePath -> [Text] -> Query -> IO Resource
fetchResource titles nd path query = case path of
        [] -> return . missingResource
              $ missingBecause
                .~ Moved Permanently
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
fileResource _titles _nd _path _query = fail "Unimplemented"


breadcrumbsFromTitles :: NonEmpty Text -> Breadcrumbs
breadcrumbsFromTitles = zipWith (flip (,)) hrefs . map toHtml . F.toList where
        hrefs = map toValue $ ("./" :: Text) : iterate ("../" <>) "../"
