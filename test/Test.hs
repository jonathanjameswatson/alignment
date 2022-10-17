import           AlignmentOptions               ( alignmentOptions )
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Options.Applicative            ( execParserPure
                                                , getParseResult
                                                , info
                                                , prefs
                                                )
import           System.FilePath                ( replaceExtension
                                                , takeBaseName
                                                )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Golden              ( findByExtension
                                                , goldenVsString
                                                )

import           Matrix                         ( make )
import           Trace                          ( trace )

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  argsFiles <- findByExtension [".txt"] "test/golden"
  return $ testGroup
    "Alignment golden tests"
    [ goldenVsString
        (takeBaseName argsFile)
        (replaceExtension argsFile ".golden")
        (do
          args <- words <$> readFile argsFile
          case
              getParseResult $ execParserPure (prefs mempty)
                                              (info alignmentOptions mempty)
                                              args
            of
              Just a -> do
                let (s1, s2) = trace . make $ a
                return $ BLU.fromString (s1 ++ "\n" ++ s2 ++ "\n")
              Nothing -> return $ BLU.fromString "Error parsing args"
        )
    | argsFile <- argsFiles
    ]
