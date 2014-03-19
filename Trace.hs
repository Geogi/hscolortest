module Trace(ftraceIO, ftrace) where

import System.IO.Unsafe

import Format

ftraceIO :: FShow s => s -> IO ()
ftraceIO msg = do
  fprint (lconcat (fshow msg) "\n")

ftrace :: FShow s => s -> a -> a
ftrace string expr = unsafePerformIO $ do
  ftraceIO string
  return expr
