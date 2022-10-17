module Lib
  ( run
  ) where

import           Matrix                         ( make )
import           Trace                          ( trace )

run :: IO ()
run = do
  let m        = make "zx12cv34" "iop1234" 1 1 1
  let (s1, s2) = trace m
  print s1
  print s2
