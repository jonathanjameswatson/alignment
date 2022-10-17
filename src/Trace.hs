{-# LANGUAGE TemplateHaskell #-}

module Trace
  ( trace
  ) where

import           Control.Lens
import           Control.Monad.State            ( State
                                                , execState
                                                )

import           Matrix

data Trace = Trace
  { _result1, _result2 :: String
  , _posI, _posJ       :: Int
  }
makeLenses ''Trace

appendToResults :: Char -> Char -> State Trace ()
appendToResults c1 c2 = do
  result1 %= (c1 :)
  result2 %= (c2 :)

trace' :: Matrix -> State Trace ()
trace' m = do
  i <- use posI
  j <- use posJ
  case m ^. cells ^?! cellAt i j ^. direction of
    Nothing -> return ()
    Just d  -> do
      case d of
        LeftDirection -> do
          posI %= pred
          appendToResults (m ^. w1 ^?! ix i) '-'
        UpDirection -> do
          posJ %= pred
          appendToResults '-' (m ^. w2 ^?! ix j)
        DiagonalDirection -> do
          posI %= pred
          posJ %= pred
          appendToResults (m ^. w1 ^?! ix i) (m ^. w2 ^?! ix j)
      trace' m

trace :: Matrix -> (String, String)
trace m =
  let result = execState
        (trace' m)
        (Trace ""
               ""
               (m ^. w1 . to length . to pred)
               (m ^. w2 . to length . to pred)
        )
  in  (result ^. result1, result ^. result2)
