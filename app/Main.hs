module Main where

import qualified Data.Ini                      as Ini
import qualified Data.Patent.Citation.Format   as Format
import qualified Data.Patent.Citation.Parse    as Parse
import qualified Data.Patent.Providers.EPO     as EPO
import qualified Data.Patent.Providers.EPO.PDF as EPO
import qualified Data.Patent.Types             as Patent
import           Data.String.Conversions       (convertString)
import qualified Data.Text                     as T
import           Options
import           Protolude
import           System.Directory              (getHomeDirectory)
import           System.FilePath               (joinPath, splitPath)
import           System.IO                     (BufferMode (NoBuffering),
                                                hSetBuffering, stdout)
import           System.IO.Error
import           Text.Printf                   (printf)

data PatentOptions = PatentOptions
  { consumerKey :: [Char]
  , secretKey   :: [Char]
  , strict      :: Bool
  , debug       :: Bool
  , configPath  :: [Char]
  }

instance Options PatentOptions where
  defineOptions =
    pure PatentOptions <*>
    simpleOption "consumerKey" "" "Consumer Key from EPO OPS" <*>
    simpleOption "secretKey" "" "Secret Key from EPO OPS" <*>
    simpleOption
      "strict"
      True
      "Limit retrieved documents to specific EPODOC input" <*>
    simpleOption "debug" False "Display debugging messages" <*>
    simpleOption
      "configPath"
      "~/.patent-api-config"
      "Path for configuration file"

getFullPath :: FilePath -> IO FilePath
getFullPath path =
  withPathComponents . replaceHome <$> getHomeDirectory <*> return path
  where
    replaceHome p ("~/":t) = p : t
    replaceHome _ s        = s
    withPathComponents f = joinPath . f . splitPath

pageProgress :: EPO.PageProgress
pageProgress total curr = printf "[%i/%i] " curr total

perInstance :: EPO.Instance -> EPO.Session ()
perInstance epodocInstance = do
  let epodoc = Format.asEPODOC $ snd epodocInstance
  liftIO $ printf "Downloading %s: " epodoc
  EPO.downloadCitationInstance pageProgress epodocInstance
  liftIO $ printf "\n"

getCredentials :: PatentOptions
               -> Maybe (Either [Char] Ini.Ini)
               -> EPO.Credentials
getCredentials opts (Just (Right ini)) =
  case ( Ini.lookupValue "EPO" "consumerKey" ini
       , Ini.lookupValue "EPO" "secretKey" ini) of
    (Right c, Right s) -> EPO.Credentials c s
    _ ->
      EPO.Credentials (T.pack . consumerKey $ opts) (T.pack . secretKey $ opts)
getCredentials opts _ =
  EPO.Credentials (T.pack . consumerKey $ opts) (T.pack . secretKey $ opts)

downloadInstancesFor :: Bool -> Patent.Citation -> EPO.Session ()
downloadInstancesFor strictness citation = do
  instances <- EPO.getCitationInstances strictness citation
  forM_ instances perInstance

parseInput :: [[Char]] -> [Patent.Citation]
parseInput args = rights $ Parse.parseCitation . T.pack <$> args

main :: IO ()
main =
  runCommand $ \opts args -> do
    configFile <- getFullPath (configPath opts)
    config <- tryJust (guard . isDoesNotExistError) $ Ini.readIniFile configFile
    hSetBuffering stdout NoBuffering
    when (null args) $
      die "You must enter at least one patent document number.\n"
    let todo = parseInput args
        credentials = getCredentials opts (rightToMaybe config)
        logLevel =
          if debug opts
            then EPO.LevelDebug
            else EPO.LevelWarn
    when (null args) $ die "You must enter at least one valid patent number.\n"
    EPO.withSession credentials EPO.v32 logLevel $
      forM_ todo (downloadInstancesFor (strict opts))
