module Main where

import qualified Data.Patent.Citation.Format   as Format
import qualified Data.Patent.Citation.Parse    as Parse
import qualified Data.Patent.Providers.EPO     as EPO
import qualified Data.Patent.Providers.EPO.PDF as EPO
import           Data.String.Conversions       (convertString)
import qualified Data.Text                     as T
import           Options
import           Protolude
import           System.IO                     (BufferMode (NoBuffering),
                                                hSetBuffering, stdout)
import           Text.Printf                   (printf)

data PatentOptions = PatentOptions
  { consumerKey :: [Char]
  , secretKey   :: [Char]
  , strict      :: Bool
  , debug       :: Bool
  }

instance Options PatentOptions where
  defineOptions =
    pure PatentOptions <*>
    simpleOption "consumerKey" "" "Consumer Key from EPO OPS" <*>
    simpleOption "secretKey" "" "Secret Key from EPO OPS" <*>
    simpleOption
      "strict"
      True
      "Limit retrived documents to specific EPODOC input" <*>
    simpleOption "debug" False "Display debugging messages"

pageProgress :: EPO.PageProgress
pageProgress total curr = printf "[%i/%i] " curr total

perInstance :: EPO.Instance -> EPO.Session ()
perInstance epodocInstance = do
  let epodoc = Format.asEPODOC $ snd epodocInstance
  liftIO $ printf "Downloading %s: " epodoc
  EPO.downloadCitationInstance pageProgress epodocInstance

main :: IO ()
main =
  runCommand $ \opts args -> do
    hSetBuffering stdout NoBuffering
    when (length args == 0) $
      die "You must enter at least one patent document number.\n"
    let parse = Parse.parseCitation . convertString $ headDef "" args
        credentials =
          EPO.Credentials
            (T.pack . consumerKey $ opts)
            (T.pack . secretKey $ opts)
        logLevel =
          if debug opts
            then EPO.LevelDebug
            else EPO.LevelWarn
    case parse of
      (Left err) -> do
        die $ printf "Input format error: %s\n" (show err :: [Char])
      (Right epodoc) ->
        void $
        EPO.withSession credentials EPO.v31 logLevel $ do
          instances <- EPO.getCitationInstances (strict opts) epodoc
          forM_ instances perInstance
