module Format (format, compose, fprint,
               Bold (Bold), Underline (Underline), Standout (Standout),
               System.Console.Terminfo.Color.Color (..)) where

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
  unset = restoreDefaultColors >>= \ua -> return (\_ -> ua)
  with  = withForegroundColor >>= \wa -> return (\c s -> wa c s)

data Bold = Bold
instance Format Bold where
  set  = boldOn >>= \sa -> return (\_ -> sa)
  with = withBold >>= \wa -> return (\_ s -> wa s)

data Underline = Underline
instance Format Underline where
  set   = enterUnderlineMode >>= \sa -> return (\_ -> sa)
  unset = exitUnderlineMode >>= \ua -> return (\_ -> ua)
  with  = withUnderline >>= \wa -> return (\_ s -> wa s)

data Standout = Standout
instance Format Standout where
  set   = enterStandoutMode >>= \sa -> return (\_ -> sa)
  unset = exitStandoutMode >>= \ua -> return (\_ -> ua)
  with  = withStandout >>= \wa -> return (\_ s -> wa s)

data Formatted = Formatted String (Capability TermOutput)

format :: Format fmt => String -> fmt -> Formatted
format s fmt = Formatted s (with >>= \wa -> return (wa fmt (termText s)))

compose :: Format fmt => Formatted -> fmt -> Formatted
compose (Formatted sn sf) fmt = Formatted sn (
  set >>= \sa -> sf >>= \fa -> unset >>= \ua ->
   return (sa fmt <#> fa <#> ua fmt))

fprint :: Formatted -> IO ()
fprint (Formatted plain formatted) = do
  term <- setupTermFromEnv
  case getCapability term (formatted) of
    Just to -> runTermOutput term to
    Nothing -> putStr plain
