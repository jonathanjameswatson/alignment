{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Matrix
  ( Direction (..),
    Cell (..),
    score,
    direction,
    Matrix (..),
    w1,
    w2,
    cells,
    cellAt,
    make,
  )
where

import AlignmentOptions
  ( AlignmentOptions,
    gapPenalty,
    matchScore,
    mismatchScore,
    word1,
    word2,
  )
import Control.Lens
import Data.Foldable (maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Direction = DiagonalDirection | UpDirection | LeftDirection deriving (Show)

data Cell = Cell
  { _score :: Int,
    _direction :: Maybe Direction
  }
  deriving (Show)

makeLenses ''Cell

data Matrix = Matrix
  { _w1, _w2 :: String,
    _cells :: Vector (Vector Cell)
  }
  deriving (Show)

makeLenses ''Matrix

cellAt :: Int -> Int -> Traversal' (Vector (Vector Cell)) Cell
cellAt i j = ix j . ix i

make :: AlignmentOptions -> Matrix
make alignmentOptions = Matrix prefixedWord1 prefixedWord2 c
  where
    prefixedWord1 = '@' : alignmentOptions ^. word1
    prefixedWord2 = '@' : alignmentOptions ^. word2
    makeCell :: Vector (Vector Cell) -> Int -> Int -> Cell
    makeCell c' i j =
      let leftCell = c' ^? cellAt (i - 1) j
          upCell = c' ^? cellAt i (j - 1)
          diagonalCell = c' ^? cellAt (i - 1) (j - 1)
       in case (leftCell, upCell) of
            (Nothing, Nothing) -> Cell 0 Nothing
            (Just _, Nothing) -> Cell (-i * alignmentOptions ^. gapPenalty) (Just LeftDirection)
            (Nothing, Just _) -> Cell (-j * alignmentOptions ^. gapPenalty) (Just UpDirection)
            (Just lc, Just uc) ->
              let scores =
                    [ ( lc ^. score - alignmentOptions ^. gapPenalty,
                        LeftDirection
                      ),
                      (uc ^. score - alignmentOptions ^. gapPenalty, UpDirection),
                      ( fromJust diagonalCell
                          ^. score
                          + alignmentOptions
                          ^. ( if prefixedWord1 ^? ix i == prefixedWord2 ^? ix j
                                 then matchScore
                                 else mismatchScore
                             ),
                        DiagonalDirection
                      )
                    ]
                  (maxScore, d) = maximumBy (comparing fst) scores
               in Cell maxScore (Just d)
    c =
      V.map
        ( \j ->
            V.map (\i -> makeCell c i j) $
              V.fromList [0 .. length prefixedWord1]
        )
        $ V.fromList [0 .. length prefixedWord2]
