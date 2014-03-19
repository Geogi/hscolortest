module Format (format, fcompose, fconcat, lconcat, rconcat,
               fshow, fprint, fprintln,
               Bold (Bold), Underline (Underline), Standout (Standout),
               System.Console.Terminfo.Color.Color (..),
               FShow) where

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
instance FShow Formatted where
  fshow = id

format :: Format fmt => String -> fmt -> Formatted
format s fmt = Formatted s (with >>= \wa -> return (wa fmt (termText s)))

fcompose :: Format fmt => Formatted -> fmt -> Formatted
fcompose (Formatted sn sf) fmt = Formatted sn (
  set >>= \sa -> sf >>= \fa -> unset >>= \ua ->
   return (sa fmt <#> fa <#> ua fmt))

fconcat :: Formatted -> Formatted -> Formatted
fconcat (Formatted sn1 sf1) (Formatted sn2 sf2) = Formatted (sn1 ++ sn2) (
  sf1 >>= \fa1 -> sf2 >>= \fa2 -> return (fa1 <#> fa2))

lconcat :: Formatted -> String -> Formatted
lconcat fs s = fconcat fs (Formatted s (return (termText s)))

rconcat :: String -> Formatted -> Formatted
rconcat s fs = fconcat (Formatted s (return (termText s))) fs

class FShow fs where
  fshow    :: fs -> Formatted
  fprint   :: fs -> IO ()
  fprintln :: fs -> IO ()
  
  fprint fs =
    let (Formatted plain formatted) = fshow fs in do
    term <- setupTermFromEnv
    case getCapability term (formatted) of
      Just to -> runTermOutput term to
      Nothing -> putStr plain

  fprintln fs = fprint (lconcat (fshow fs) "\n")
