module RedError(RedError (RedError), OrangeComp,
                throwError, catchError) where

import Control.Monad.Error
import Data.List

import Format

data RedError = RedError String String
instance Error RedError where
  noMsg    = RedError "Error:" "unspecified error"
  strMsg s = case elemIndex ':' s of
    Just i  -> RedError h t where
      (h, t) = splitAt (i + 1) s
    Nothing -> RedError "Error:" s
instance FShow RedError where
  fshow (RedError h t) = (fconcat (compose (format h Red) Bold)
                          (format (" " ++ t) Red))

type OrangeComp = Either RedError
