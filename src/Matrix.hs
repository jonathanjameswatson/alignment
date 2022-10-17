{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Matrix
  ( Direction(..)
  , Cell(..)
  , score
  , direction
  , Matrix(..)
  , w1
  , w2
  , cells
  , cellAt
  , make
  ) where

import           Control.Lens
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

make :: String -> String -> Int -> Int -> Int -> Matrix
make word1 word2 orthogonalPenalty diagonalPenalty matchScore = Matrix
  prefixedWord1
  prefixedWord2
  c
 where
  prefixedWord1 = '@' : word1
  prefixedWord2 = '@' : word2
  makeCell :: [[Cell]] -> Int -> Int -> Cell
  makeCell c' i j =
    let leftCell     = c' ^? cellAt (i - 1) j
        upCell       = c' ^? cellAt i (j - 1)
        diagonalCell = c' ^? cellAt (i - 1) (j - 1)
    in  case (leftCell, upCell) of
          (Nothing, Nothing) -> Cell 0 Nothing
          (Just _ , Nothing) -> Cell (-i) (Just LeftDirection)
          (Nothing, Just _ ) -> Cell (-j) (Just UpDirection)
          (Just lc, Just uc) ->
            let scores =
                  [ (lc ^. score - orthogonalPenalty, LeftDirection)
                  , (uc ^. score - orthogonalPenalty, UpDirection)
                  , ( fromJust diagonalCell
                      ^. score
                      +  (if prefixedWord1 ^? ix i == prefixedWord2 ^? ix j
                           then matchScore
                           else (-diagonalPenalty)
                         )
                    , DiagonalDirection
                    )
                  ]
                (maxScore, d) = maximumBy (comparing fst) scores
            in  Cell maxScore (Just d)
  c =
    [ [ makeCell c i j | i <- [0 .. length prefixedWord1 - 1] ]
    | j <- [0 .. length prefixedWord2 - 1]
    ]
