module Main where

import           Control.Lens.Operators

import qualified Data.Ini                      as Ini
import qualified Data.Patent.Citation.Format   as Format
import qualified Data.Patent.Citation.Parse    as Parse
import qualified Data.Patent.Providers.EPO     as EPO
import qualified Data.Patent.Providers.EPO.PDF as EPO
import qualified Data.Patent.Types             as Patent
import qualified Data.Text                     as T
import           Options
import           Protolude
import           System.Directory              (getHomeDirectory)
import           System.FilePath               (joinPath, splitPath)
import           System.IO                     (BufferMode (NoBuffering),
                                                hSetBuffering, stdout)
import           System.IO.Error
import           Text.CSV
import           Text.Printf                   (printf)

data PatentOptions = PatentOptions
  { consumerKey :: [Char]
  , secretKey   :: [Char]
  , strict      :: Bool
  , debug       :: Bool
  , configPath  :: [Char]
  , csvMode     :: Bool
  , csvColumn   :: Int
  , writeReport :: [Char]
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
      "Path for configuration file" <*>
    simpleOption "csvMode" False "Load citations from CSV file" <*>
    simpleOption "csvColumn" 0 "Column of CSV with citations" <*>
    simpleOption
      "writeReport"
      ""
      "Export a report in CSV format of downloaded patents, including bibliographic information"

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
  let epodoc = Format.asEPODOC $ epodocInstance ^. EPO.fullCitation
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
  if (null instances)
    then do
      liftIO $ printf "%s\tNo results found.\n" (Format.asEPODOC citation)
      return ()
    else do
      forM_ instances perInstance

parseInput :: [Char] -> IO (Maybe Patent.Citation)
parseInput arg =
  either
    (const $ printf "Error parsing \"%s\"\n" arg >> return Nothing)
    (return . Just) $
  Parse.parseCitation . T.pack $ arg

extractColumn :: Int -> [[Field]] -> [[Char]]
extractColumn column = map (\row -> atDef "" row column)

reportOn :: Patent.Citation -> EPO.Session Record
reportOn citation = do
  bib <- EPO.getBibliography "en" citation
  pure $
    T.unpack <$>
    [ Format.asEPODOC citation
    , bib ^. Patent.biblioTitle
    , bib ^. Patent.biblioPubDate
    , bib ^. Patent.biblioAppDate
    , Format.asEPODOC $ bib ^. Patent.biblioAppCitation
    , T.intercalate "; " $ bib ^. Patent.biblioApplicants
    , T.intercalate "; " $ bib ^. Patent.biblioInventors
    , bib ^. Patent.biblioFamilyID
    , T.intercalate "; " $
      Format.asEPODOC <$> bib ^. Patent.biblioPriorityCitations
    ]

main :: IO ()
main =
  runCommand $ \opts args -> do
    configFile <- getFullPath (configPath opts)
    config <- tryJust (guard . isDoesNotExistError) $ Ini.readIniFile configFile
    hSetBuffering stdout NoBuffering
    input <-
      if csvMode opts
        then do
          file <- parseCSVFromFile (headDef "" args)
          either
            (const $ die "Error parsing CSV file\n" >> return [])
            (return . (extractColumn (csvColumn opts)))
            file
        else return args
    citations <- catMaybes <$> mapM parseInput input
    let credentials = getCredentials opts (rightToMaybe config)
        logLevel =
          if debug opts
            then EPO.LevelDebug
            else EPO.LevelWarn
    when (null args) $
      die "You must enter at least one patent document number.\n"
    EPO.withSession credentials EPO.v32 logLevel $ do
      forM_ citations (downloadInstancesFor (strict opts))
      when (length (writeReport opts) > 0) $ do
        csvReport <- mapM reportOn citations
        liftIO $ writeFile (writeReport opts) $ T.pack $ printCSV csvReport
