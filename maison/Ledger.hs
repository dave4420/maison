module Ledger (ledgerSite) where

-- base
import           Control.Arrow
import qualified Data.Foldable               as F
import           Data.List
import           Data.Monoid
import           Data.String

-- blaze-html
import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as HT
import qualified Text.Blaze.Html5.Attributes as AT

-- hledger-lib
import qualified Hledger                     as LEDGER

-- maison
import           Http

-- old-locale
import           System.Locale

-- text
import qualified Data.Text              as T

-- time
import           Data.Time.Format


ledgerSite :: FilePath -> Site
ledgerSite nf = Site $ \path _query -> case path of
        []            -> go Nothing
        [accountName] -> go (Just accountName)
        _             -> return (Left defaultMissingResource)
    where
        go sub = do
                journal
                 <- either fail return
                    =<< LEDGER.readJournalFile Nothing Nothing nf
                let ledger = LEDGER.ledgerFromJournal
                             (LEDGER.Empty True)
                             journal
                return $ case sub of
                        Nothing -> goJournal journal
                        Just ac -> maybe (Left defaultMissingResource)
                                         (goAccount journal acName)
                                         (LEDGER.ledgerAccount ledger acName)
                            where
                                acName = T.unpack ac
        goJournal journal = Right found {existingGet = Just $ balances journal}
        goAccount journal acName _ac
                = Right found {existingGet = Just $ transactions journal acName}
        found = defaultExistingResource :: ExistingResource


balances :: LEDGER.Journal -> IO Entity
balances journal
        = return . entityFromHtml . html5Page title $ nav <> table
    where
        (title, nav) = titleNav [] "102 Richmond Road Accounts"
        report = LEDGER.balanceReport
                 LEDGER.defreportopts {LEDGER.flat_ = True,
                                       LEDGER.empty_ = True}
                 LEDGER.Any
                 journal
        table = HT.table . mconcat . map row $ fst report
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


transactions :: LEDGER.Journal -> String -> IO Entity
transactions journal acName
        = return . entityFromHtml . html5Page title
          $ nav <> table
    where
        (title, nav) = titleNav [("102 Richmond Road Accounts", "./")]
                                (fromString acName)
        report = LEDGER.postingsReport
                 LEDGER.defreportopts {LEDGER.flat_ = True}
                 (LEDGER.Acct acName)
                 journal
        table = HT.table
                . mconcat
                . (HT.tr (F.foldMap (HT.td ! AT.style "text-align:right" $)
                          ["", "", "Credits", "Debits", "Balance"])
                   :)
                . map row
                . reverse
                $ snd report
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


html5Page :: Html -> Html -> Html
html5Page title body
        = HT.docTypeHtml
          $ HT.head (HT.title title)
            <> HT.body body

titleNav :: [(Html, AttributeValue)] -> Html -> (Html, Html)
titleNav above here = (title, nav) where
        title = mconcat . intersperse " \x2190 "
                $ here : map fst (reverse above)
        nav = HT.nav
              . mconcat
              . intersperse " \x2192 "
              . map link
              $ (map . second) Just above ++ [(here, Nothing)]
        link (anchor, href)
                = HT.i
                  $ maybe anchor
                          (\href' -> HT.a ! AT.href href' $ anchor)
                          href
