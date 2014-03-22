module Ledger (ledgerSite, ledgerFileResource) where

import           Page

-- base
import           Control.Applicative
import qualified Data.Foldable               as F
import           Data.List
import           Data.Monoid
import           Data.String

-- blaze-html
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as HT
import qualified Text.Blaze.Html5.Attributes as AT

-- containers
import qualified Data.Set                    as S

-- hledger-lib
import qualified Hledger                     as LEDGER

-- lens
import qualified Control.Lens                as L
import           Control.Lens.Operators

-- maison
import           Http

-- old-locale
import           System.Locale

-- semigroups
import qualified Data.List.NonEmpty          as SG

-- text
import           Data.Text (Text)
import qualified Data.Text              as T

-- time
import           Data.Time


ledgerSite :: Text -> FilePath -> Site
ledgerSite title nf
        = sealSiteNoAuth $ ledgerFileResource (pure title) nf . SG.toList

ledgerFileResource
        :: NonEmpty Text -> FilePath -> [Text] -> Query -> IO Resource
ledgerFileResource titles nf path query = case path of
        []            -> goInside
        [""]          -> go Nothing
        [accountName] -> go (Just accountName)
        _             -> return $ missingResource id
    where
        goInside = return . missingResource
                   $ missingBecause
                     .~ moved Permanently
                              (RelPathUri 0 (SG.head titles :| [""]) query)
        go sub = do
                journal
                 <- either fail return
                    =<< LEDGER.readJournalFile Nothing Nothing nf
                let ledger = LEDGER.ledgerFromJournal
                             (LEDGER.Empty True)
                             journal
                return $ case sub of
                        Nothing -> goJournal journal
                        Just ac -> maybe (missingResource id)
                                         (goAccount journal acName)
                                         (LEDGER.ledgerAccount ledger acName)
                            where
                                acName = T.unpack ac
        goJournal journal = existingResource $ existingGet .~ Just balances'
            where
                balances' = balances breadcrumbs journal
        goAccount journal acName _ac
                = existingResource $ existingGet .~ Just transactions'
            where
                transactions' = transactions breadcrumbs' journal acName
                breadcrumbs' = breadcrumbsFromTitles'
                               $ (T.pack acName) SG.<| titles
        breadcrumbs = breadcrumbsFromTitles titles


breadcrumbsFromTitles :: NonEmpty Text -> Breadcrumbs
breadcrumbsFromTitles = zipWith (flip (,)) hrefs . map toHtml . F.toList where
        hrefs = map toValue $ ("./" :: Text) : iterate ("../" <>) "../"

breadcrumbsFromTitles' :: NonEmpty Text -> Breadcrumbs  -- for file
breadcrumbsFromTitles' titles
        = zipWith (flip (,)) hrefs . map toHtml . F.toList $ titles
    where
        hrefs = map toValue $ "./" <> SG.head titles
                              : "./"
                              : iterate ("../" <>) "../"


balances :: Breadcrumbs -> LEDGER.Journal -> IO Entity
balances breadcrumbs journal = do
        today <- localDay . zonedTimeToLocalTime <$> getZonedTime
        return . entityFromPage breadcrumbs $ table today
     where

        report today
                = LEDGER.balanceReport
                  LEDGER.defreportopts {LEDGER.flat_ = True,
                                        LEDGER.empty_ = True}
                  (LEDGER.Date $ LEDGER.DateSpan Nothing (Just tomorrow))
                  journal
            where
                tomorrow = addDays 1 today

        table = HT.table . mconcat . map row . leafAccountsOnly . fst . report

        leafAccountsOnly accts
                = filter (\acct -> S.notMember (acct ^. L._1) unwanted) accts
            where
                unwanted = S.fromList . concatMap (^. L._1 . L.to parents)
                           $ accts
                parents s = case dropWhileEnd (':' /=) s of
                        "" -> []
                        s' -> let s'' = init s' in s'' : parents s''

        row :: LEDGER.BalanceReportItem -> Html
        row (fullName, _shortName, _indent, amount)
                = HT.tr
                  . mconcat
                  $ [HT.td ! AT.style "text-align:left"
                     $ HT.a ! AT.href (fromString $ "./" ++ fullName)
                       $ fromString fullName,
                     HT.td ! AT.style "text-align:right"
                     $ balance,
                     HT.td ! AT.style "text-align:left"
                     $ tag]
            where
                (balance, tag) = formatAmountWithTag amount


transactions :: Breadcrumbs -> LEDGER.Journal -> String -> IO Entity
transactions breadcrumbs journal acName = do
        today <- localDay . zonedTimeToLocalTime <$> getZonedTime
        return . entityFromPage breadcrumbs $ table today
    where

        report today
                = LEDGER.postingsReport
                  LEDGER.defreportopts {LEDGER.flat_ = True}
                  (LEDGER.And
                   [LEDGER.Acct acName,
                    LEDGER.Date $ LEDGER.DateSpan Nothing (Just tomorrow)])
                  journal
            where
                tomorrow = addDays 1 today

        table = HT.table
                . mconcat
                . (HT.tr (F.foldMap (HT.td ! AT.style "text-align:right" $)
                          ["", "", "Credits", "Debits", "Balance"])
                   :)
                . map row
                . reverse
                . snd
                . report

        row :: LEDGER.PostingsReportItem -> Html
        row (when, what, LEDGER.Posting {pamount = pamount}, amount)
                = HT.tr
                  . mconcat
                  $ [HT.td ! AT.style "text-align:left"
                     $ maybe "" (fromString . fmtDate) when,
                     HT.td ! AT.style "text-align:left"
                     $ maybe "" fromString what,
                     HT.td ! AT.style "text-align:right"
                     $ tshowNonZeroMixedAmount (negate
                                               $ filterAmount isCredit pamount),
                     HT.td ! AT.style "text-align:right"
                     $ tshowNonZeroMixedAmount (filterAmount isDebit pamount),
                     HT.td ! AT.style "text-align:right" $ balance,
                     HT.td ! AT.style "text-align:left" $ tag]
            where
                (balance, tag) = formatAmountWithTag amount
                fmtDate = formatTime defaultTimeLocale "%e %B %Y"


formatAmountWithTag :: LEDGER.MixedAmount -> (Html, Html)
formatAmountWithTag amount
        | allAmount isCredit amount       = (tshowMixedAmount $ negate amount,
                                             "CR")
        | allAmount isDebit amount        = (tshowMixedAmount amount, "DR")
        | LEDGER.isZeroMixedAmount amount = ("0", "")
        | otherwise                       = (tshowMixedAmount amount, "DR?")

tshowMixedAmount, tshowNonZeroMixedAmount
        :: IsString a => LEDGER.MixedAmount -> a
tshowMixedAmount = fromString . LEDGER.showMixedAmount
tshowNonZeroMixedAmount (LEDGER.Mixed []) = ""
tshowNonZeroMixedAmount amount            = tshowMixedAmount amount

allAmount :: (LEDGER.Amount -> Bool) -> LEDGER.MixedAmount -> Bool
allAmount p (LEDGER.Mixed amounts) = all p amounts

filterAmount
        :: (LEDGER.Amount -> Bool) -> LEDGER.MixedAmount -> LEDGER.MixedAmount
filterAmount p (LEDGER.Mixed amounts) = LEDGER.Mixed (filter p amounts)

isCredit, isDebit :: LEDGER.Amount -> Bool
isCredit LEDGER.Amount{..} = aquantity < 0
isDebit  LEDGER.Amount{..} = 0 < aquantity
