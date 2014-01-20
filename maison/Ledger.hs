module Ledger (ledgerSite) where

-- base
import           Control.Applicative
import qualified Data.Foldable          as F
import           Data.Monoid

-- hledger-lib
import qualified Hledger                as LEDGER

-- maison
import           Http

-- old-locale
import           System.Locale

-- text
import           Data.Text (Text)
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


balances :: LEDGER.Journal -> IO ([ExtraHeader], Entity)
balances journal = return ([], Entity{..}) where
        report = LEDGER.balanceReport
                 LEDGER.defreportopts {LEDGER.flat_ = True, LEDGER.empty_ = True}
                 LEDGER.Any --(LEDGER.Empty True)
                 journal
        entityType = "text/html; charset=utf-8"
        entityBody = entityBodyFromStrictText
                     . htmlElement "table"
                     . map row
                     $ fst report
        row :: LEDGER.BalanceReportItem -> Text
        row (fullName, _shortName, _indent, amount)
                = htmlElement "tr"
                  [td [alignLeft] . pure
                   $ aHref . ("./" <>) <*> pure
                     $ T.pack fullName,
                   td [alignRight] [balance],
                   td [alignLeft] [tag]]
            where
                (balance, tag) = formatAmountWithTag amount


transactions :: LEDGER.Journal -> String -> IO ([ExtraHeader], Entity)
transactions journal acName = return ([], Entity{..}) where
        report = LEDGER.postingsReport
                 LEDGER.defreportopts {LEDGER.flat_ = True}
                 (LEDGER.Acct acName)
                 journal
        entityType = "text/html; charset=utf-8"
        entityBody = entityBodyFromStrictText
                     . htmlElement "table"
                     . (htmlElement "tr" (map (td [alignRight] . pure)
                                          ["", "", "Credits", "Debits", "Balance"])
                        :)
                     . map row
                     . reverse
                     $ snd report
        row :: LEDGER.PostingsReportItem -> Text
        row (when, what, LEDGER.Posting {pamount = pamount}, amount)
                = htmlElement "tr"
                  [td [alignLeft] [maybe "" (T.pack . formatTime defaultTimeLocale "%e %B %Y") when],
                   td [alignLeft] [maybe "" T.pack what],
                   td [alignRight] [tshowNonZeroMixedAmount . negate
                                    $ filterAmount isCredit pamount],
                   td [alignRight] [tshowNonZeroMixedAmount
                                    $ filterAmount isDebit pamount],
                   td [alignRight] [balance],
                   td [alignLeft] [tag]]
            where
                (balance, tag) = formatAmountWithTag amount


formatAmountWithTag :: LEDGER.MixedAmount -> (Text, Text)
formatAmountWithTag amount
        | allAmount isCredit amount       = (tshowMixedAmount $ negate amount, "CR")
        | allAmount isDebit amount        = (tshowMixedAmount amount, "DR")
        | LEDGER.isZeroMixedAmount amount = ("0", "")
        | otherwise                       = (tshowMixedAmount amount, "DR?")

tshowMixedAmount, tshowNonZeroMixedAmount :: LEDGER.MixedAmount -> Text
tshowMixedAmount = T.pack . LEDGER.showMixedAmount
tshowNonZeroMixedAmount (LEDGER.Mixed []) = ""
tshowNonZeroMixedAmount amount            = tshowMixedAmount amount

allAmount :: (LEDGER.Amount -> Bool) -> LEDGER.MixedAmount -> Bool
allAmount p (LEDGER.Mixed amounts) = all p amounts

filterAmount :: (LEDGER.Amount -> Bool) -> LEDGER.MixedAmount -> LEDGER.MixedAmount
filterAmount p (LEDGER.Mixed amounts) = LEDGER.Mixed (filter p amounts)

isCredit, isDebit :: LEDGER.Amount -> Bool
isCredit LEDGER.Amount{..} = aquantity < 0
isDebit  LEDGER.Amount{..} = 0 < aquantity


aHref :: Text -> [Text] -> Text
aHref href = htmlElement' "a" [(Just "href", href)]

td :: [(Maybe Text, Text)] -> [Text] -> Text
td = htmlElement' "td"

alignLeft, alignRight :: (Maybe Text, Text)
alignLeft = (Just "align", "left")
alignRight = (Just "align", "right")

htmlElement :: Text -> [Text] -> Text
htmlElement = flip htmlElement' []

htmlElement' :: Text -> [(Maybe Text, Text)] -> [Text] -> Text
htmlElement' tag attrs body
        = (mconcat . concat)
          [["<", tag, F.foldMap attr attrs, ">"], body, ["</", tag, ">"]]
    where
        attr (Nothing, value) = " " <> value
        attr (Just name, value) = mconcat [" ", name, "=\"", value, "\""]
