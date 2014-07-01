module PageSite (pageSite) where

-- base
import           Data.Monoid

-- maison
import           Http


pageSite :: FilePath -> Site
pageSite _root = mempty