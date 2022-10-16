{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib
  ( run
  ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable                  ( maximumBy )
import           Data.Maybe                     ( fromJust )
import           Data.Ord                       ( comparing )

data Direction = DiagonalDirection | UpDirection | LeftDirection deriving (Show)

data Cell = Cell
  { _score     :: Int
  , _direction :: Maybe Direction
  }
  deriving Show
makeLenses ''Cell

data Matrix = Matrix
  { _w1, _w2 :: String
  , _cells   :: [[Cell]]
  }
  deriving Show
makeLenses ''Matrix

cellAt :: Int -> Int -> Traversal' [[Cell]] Cell
cellAt i j = ix j . ix i

getScoreAndDirection :: Cell -> Int -> Direction -> (Int, Maybe Direction)
getScoreAndDirection c x d = (c ^. score + x, Just d)

make :: String -> String -> Int -> Int -> Int -> Matrix
make word1 word2 orthogonalPenalty diagonalPenalty matchScore = Matrix
  prefixedWord1
  prefixedWord2
  c
 where
  prefixedWord1 = '@' : word1
  prefixedWord2 = '@' : word2
  c =
    [ [ let leftCell     = c ^? cellAt (i - 1) j
            upCell       = c ^? cellAt i (j - 1)
            diagonalCell = c ^? cellAt (i - 1) (j - 1)
        in  case (leftCell, upCell) of
              (Nothing, Nothing) -> Cell 0 Nothing
              (Just _ , Nothing) -> Cell (-i) (Just LeftDirection)
              (Nothing, Just _ ) -> Cell (-j) (Just UpDirection)
              (Just lc, Just uc) ->
                let scores =
                      [ getScoreAndDirection lc
                                             (-orthogonalPenalty)
                                             LeftDirection
                      , getScoreAndDirection uc (-orthogonalPenalty) UpDirection
                      , getScoreAndDirection
                        (fromJust diagonalCell)
                        (if prefixedWord1 ^? ix i == prefixedWord2 ^? ix j
                          then matchScore
                          else (-diagonalPenalty)
                        )
                        DiagonalDirection
                      ]
                    (maxScore, d) = maximumBy (comparing (^. _1)) scores
                in  Cell maxScore d
      | i <- [0 .. length prefixedWord1 - 1]
      ]
    | j <- [0 .. length prefixedWord2 - 1]
    ]


data Trace = Trace
  { _result1, _result2 :: String
  , _posI, _posJ       :: Int
  }
makeLenses ''Trace

appendToResults :: Char -> Char -> State Trace ()
appendToResults c1 c2 = do
  result1 %= (c1 :)
  result2 %= (c2 :)

traceCurrentCell :: Matrix -> State Trace ()
traceCurrentCell m = do
  i <- use posI
  j <- use posJ
  case m ^. cells ^?! cellAt i j ^. direction of
    Nothing -> appendToResults (m ^. w1 ^?! ix i) (m ^. w2 ^?! ix j)
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
      traceCurrentCell m

trace :: Matrix -> (String, String)
trace m =
  let result = execState
        (traceCurrentCell m)
        (Trace ""
               ""
               (m ^. w1 . to length . to pred)
               (m ^. w2 . to length . to pred)
        )
  in  (result ^. result1, result ^. result2)

run :: IO ()
run = do
  let m        = make "1234zxcv" "uiop1234" 1 1 1
  let (s1, s2) = trace m
  print $ tail s1
  print $ tail s2
