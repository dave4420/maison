module Ledger (ledgerSite, ledgerFileResource) where

import           Page

-- base
import           Control.Applicative
import qualified Data.Foldable               as F
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String

-- blaze-html
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as HT
import qualified Text.Blaze.Html5.Attributes as AT

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
import qualified Data.Semigroup              as SG

-- text
import           Data.Text (Text)
import qualified Data.Text              as T

-- time
import           Data.Time


ledgerSite :: Text -> FilePath -> Site
ledgerSite title nf
        = sealSiteNoAuth $ ledgerFileResource (pure title) nf . SG.toList

ledgerFileResource
        :: NonEmpty Text -> FilePath -> [Text] -> Query -> HttpIO Resource
ledgerFileResource titles nf path _query = case path of
        []            -> goInside
        [""]          -> go Nothing
        [accountName] -> go (Just accountName)
        _             -> return $ missingResource id
    where
        goInside = do
                uri <- L.view requestUri
                return . missingResource
                    $ missingBecause
                      .~ moved Permanently
                               (AbsUri . (uriPath %~ (SG.<> pure "")) $ uri)
        go sub = do
                journal
                 <- either fail return
                    =<< liftIO (LEDGER.readJournalFile Nothing Nothing nf)
                let ledger = LEDGER.ledgerFromJournal
                             (LEDGER.Empty True)
                             journal
                case sub of
                        Nothing -> goJournal journal
                        Just ac -> maybe (return $ missingResource id)
                                         (goAccount journal acName)
                                         (LEDGER.ledgerAccount ledger acName)
                            where
                                acName = T.unpack ac
        goJournal journal = do
                breadcrumbs <- breadcrumbsFromTitles titles
                return . existingResource
                    $ existingGet ?~ balances breadcrumbs journal
        goAccount journal acName _ac = do
                breadcrumbs
                 <- breadcrumbsFromTitles $ (T.pack acName) SG.<| titles
                return . existingResource
                    $ existingGet ?~ transactions breadcrumbs journal acName


balances :: Breadcrumbs -> LEDGER.Journal -> HttpIO Entity
balances breadcrumbs journal = liftIO $ do
        today <- localDay . zonedTimeToLocalTime <$> getZonedTime
        return . entityFromPage breadcrumbs $ css <> table today
     where

        report today
                = LEDGER.balanceReport
                  LEDGER.defreportopts {LEDGER.flat_ = True,
                                        LEDGER.empty_ = True}
                  (LEDGER.Date $ LEDGER.DateSpan Nothing (Just tomorrow))
                  journal
            where
                tomorrow = addDays 1 today

        table today = HT.table $ columns <> F.foldMap row (fst $ report today)

        row :: LEDGER.BalanceReportItem -> Html
        row (fullName, _shortName, _indent, amount)
                = HT.tr
                  . mconcat
                  $ [HT.td ! AT.class_ (bulletClass . length . filter (== ':')
                                        $ fullName)
                     $ HT.a ! AT.href (fromString $ "./" ++ fullName)
                       $ fromString (nodeName fullName),
                     HT.td balance,
                     HT.td tag]
            where
                (balance, tag) = formatAmountWithTag amount
                nodeName = last . groupBy ((==) `on` (== ':'))

        columns = HT.colgroup $ F.foldMap (\class_ -> HT.col ! AT.class_ class_)
                                          ["name", "magnitude", "sign"]

        bulletClass i = fromString $ "level " ++ ('o' : show i)

        css = HT.style . mconcat
              $ ".name, .sign { text-align: left; } \
               \ .magnitude { text-align: right; } \
               \ .level { display: list-item; \
                        \ list-style: disc inside; \
                        \ min-width: 8em; }"
                : zipWith padding [2,4..] [1..4]
            where
                padding :: IsString a => Int -> Int -> a
                padding left level
                        = fromString
                          $ ".o" <> show level <> " { padding-left: "
                            <> show left <> "em; }"


transactions :: Breadcrumbs -> LEDGER.Journal -> String -> HttpIO Entity
transactions breadcrumbs journal acName = liftIO $ do
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
                . groupTransactions
                . snd
                . report

        groupTransactions
                :: [LEDGER.PostingsReportItem] -> [LEDGER.PostingsReportItem]
        groupTransactions
                = mapMaybe netTransaction . filter isStartOfTransaction . tails
            where
                isStartOfTransaction (x : _) = startsTransaction x
                isStartOfTransaction _       = False
                startsTransaction (Just _, Just _, _, _) = True
                startsTransaction _                      = False
                netTransaction [] = Nothing
                netTransaction (x : xs)
                        = netTransaction'
                          $ x : takeWhile (not . startsTransaction) xs
                netTransaction' [] = Nothing
                netTransaction' xs @ ((when, what, posting, _) : _)
                    | xfer == LEDGER.Mixed []
                        = Nothing
                    | otherwise
                        = Just (when, what, posting {LEDGER.pamount = xfer},
                                last xs ^. L._4)
                    where
                        xfer = filterAmount isNonZero . sum
                               $ xs ^.. L.traverse . L._3 . L.to LEDGER.pamount

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

isCredit, isDebit, isNonZero :: LEDGER.Amount -> Bool
isCredit  LEDGER.Amount{..} = aquantity < 0
isDebit   LEDGER.Amount{..} = 0 < aquantity
isNonZero LEDGER.Amount{..} = 0 /= aquantity
