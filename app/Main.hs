module Main
  ( main,
  )
where

import AlignmentOptions
  ( AlignmentOptions (..),
    alignmentOptions,
  )
import Control.Applicative ((<**>))
import Matrix (make)
import Options.Applicative
  ( ParserInfo,
    execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
  )
import Trace (trace)

opts :: ParserInfo AlignmentOptions
opts =
  info
    (alignmentOptions <**> helper)
    ( fullDesc
        <> progDesc "Find an alignment between two strings"
        <> header
          "alignment - a program for finding an alignment between two strings"
    )

main :: IO ()
main = do
  a <- execParser opts
  let (s1, s2) = trace . make $ a
  print s1
  print s2
