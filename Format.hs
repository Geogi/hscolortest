module Format where


import Control.Monad

import System.Console.Terminfo.Base
import System.Console.Terminfo.Effects
import System.Console.Terminfo.Color

class Format fmt where
  set   :: Capability (fmt -> TermOutput)
  unset :: Capability (fmt -> TermOutput)
  with  :: Capability (fmt -> TermOutput -> TermOutput)

  unset = allAttributesOff >>= \aa -> return (\_ -> aa)
  with  = set >>= \sa -> unset >>= \ua ->
    return (\f s -> sa f <#> s <#> ua f)

instance Format Color where
  set   = setForegroundColor >>= \sa -> return (\c -> sa c)
  unset = restoreDefaultColors >>= \ra -> return (\_ -> ra)
  with  = withForegroundColor >>= \wa -> return (\c s -> wa c s)

data Bold = Bold
instance Format Bold where
  set  = boldOn >>= \ba -> return (\_ -> ba)
  with = withBold >>= \wa -> return (\_ s -> wa s)

data Underline = Underline
instance Format Underline where
  set   = enterUnderlineMode >>= \ea -> return (\_ -> ea)
  unset = exitUnderlineMode >>= \ea -> return (\_ -> ea)
  with  = withUnderline >>= \wa -> return (\_ s -> wa s)

data Standout = Standout
instance Format Standout where
  set   = enterStandoutMode >>= \ea -> return (\_ -> ea)
  unset = exitStandoutMode >>= \ea -> return (\_ -> ea)
  with  = withStandout >>= \wa -> return (\_ s -> wa s)

data Formatted = Formatted String (Capability TermOutput)

format :: Format fmt => String -> fmt -> Formatted
format s fmt = Formatted s (with >>= \wa -> return (wa fmt (termText s)))

data ComposedFormat f1 f2 = (Format f1, Format f2) => ComposedFormat f1 f2
instance Format ComposedFormat where
  set   = set >>= \sa -> return (sr sa) where
    sr sa ComposedFormat f1 f2 = sa f1 <#> sa f2
  unset = unset >>= \ua -> return (ur ua) where
    ur ua ComposedFormat f1 f2 = ua f1 <#> ua f2
  with  = set >>= \sa -> unset >>= \ua -> return (
    \fr s -> set fr <#> s <#> unset fr)


fprint :: Formatted -> IO ()
fprint (Formatted plain formatted) = do
  term <- setupTermFromEnv
  case getCapability term (formatted) of
    Just to -> runTermOutput term to
    Nothing -> putStr plain
