{-# LANGUAGE TemplateHaskell #-}

module AlignmentOptions
  ( AlignmentOptions(..)
  , word1
  , word2
  , gapPenalty
  , matchScore
  , mismatchScore
  , alignmentOptions
  ) where

import           Control.Lens
import           Options.Applicative

data AlignmentOptions = AlignmentOptions
  { _word1         :: String
  , _word2         :: String
  , _matchScore    :: Int
  , _mismatchScore :: Int
  , _gapPenalty    :: Int
  }
makeLenses ''AlignmentOptions

alignmentOptions :: Parser AlignmentOptions
alignmentOptions =
  AlignmentOptions
    <$> strArgument (metavar "STRING1" <> help "First word to align")
    <*> strArgument (metavar "STRING2" <> help "Second word to align")
    <*> option
          auto
          (  long "match-score"
          <> short 'm'
          <> metavar "INT"
          <> help "Score for matching letters"
          <> showDefault
          <> value 1
          )
    <*> option
          auto
          (  long "mismatch-score"
          <> short 'n'
          <> metavar "INT"
          <> help "Score for mismatching letters"
          <> showDefault
          <> value (-1)
          )
    <*> option
          auto
          (  long "gap-penalty"
          <> short 'g'
          <> metavar "INT"
          <> help "Penalty for gaps"
          <> showDefault
          <> value 1
          )
