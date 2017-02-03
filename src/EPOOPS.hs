{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}

{-|
Module      : EPOOPS
Description : EPOOPS's main module

-}
module EPOOPS
    ( withOPSSession,
      epoRequest,
      OPSService (..),
      Credentials (..),
      InstanceListing,
      getEPODOCInstances,
      downloadEPODOCInstance
    )
    where

import           Control.Arrow
import           Control.Lens       hiding ((&))
import           Data.Aeson.Lens
import qualified Data.ByteString    as B
import           Data.String.Here
import qualified Data.Text.Encoding as T
import           EPODOC
import           Lib.Prelude
import qualified Network.Wreq       as Wreq
import           System.IO.Streams  (fromLazyByteString)
import qualified System.IO.Streams  as S
import qualified System.IO.Temp     as Temp
import qualified Text.Parsec        as Parsec
import           Text.Printf        (printf)
import           Text.Read          (readMaybe)
import qualified Turtle             hiding ((<>))

import qualified Data.Map.Strict    as Map
import qualified Text.XML           as XML
import           Text.XML.Cursor    (($//), (>=>))
import qualified Text.XML.Cursor    as XML

opsEndPoint :: [Char]
opsEndPoint = "https://ops.epo.org/3.1"

newtype OAuth2Token = OAuth2Token { _rawtoken :: B.ByteString } deriving (Show, Eq)

newtype OPSSession a = OPSSession {
  runOPSSession :: ReaderT Credentials (StateT SessionState IO) a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Credentials, MonadState SessionState)

data Credentials = Credentials {
  consumerKey :: [Char],
  secretKey   :: [Char]
}

data SessionState = SessionState {quotaState :: Quotas, tokenState :: Maybe OAuth2Token}

data OPSServiceState = Idle | Busy | Overloaded deriving (Eq, Ord)
data OPSServiceTraffic = Green | Yellow | Red | Black deriving (Eq, Ord)
data OPSServiceQuota = RetrievalQuota | SearchQuota | INPADOCQuota | ImagesQuota | OtherQuota deriving (Eq, Ord)
data OPSService = Biblio | Abstract | FullCycle | FullText | Description | Claims | Equivalents | Images
  deriving (Eq, Ord)

type Quotas = (OPSServiceState, Map OPSServiceQuota (OPSServiceTraffic, Int))
type InstanceListing = (Int, EPODOC)

initialState :: SessionState
initialState = SessionState
                (Idle, Map.fromList [
                (RetrievalQuota, (Green, 200)),
                (SearchQuota, (Green, 30)),
                (INPADOCQuota, (Green, 60)),
                (ImagesQuota, (Green, 200)),
                (OtherQuota, (Green, 1000))])
                Nothing

withOPSSession :: Credentials -> OPSSession a -> IO (a, SessionState)
withOPSSession credentials k = runStateT (runReaderT (runOPSSession k) credentials) initialState

authenticate :: OPSSession OAuth2Token
authenticate = do
  sessionState <- get
  settings <- ask
  let token = tokenState sessionState
      client_id = consumerKey settings
      client_secret = secretKey settings
  case token of
      Just t -> return t
      Nothing -> do
         newtoken <- liftIO $ requestOAuthToken client_id client_secret
         put sessionState {tokenState = Just newtoken}
         return newtoken

epoRequest :: EPODOC -> OPSService -> OPSSession XML.Document
epoRequest epodoc service = do
  sessionState <- get
  token <- authenticate
  let opts =  Wreq.defaults
            & Wreq.auth ?~ Wreq.oauth2Bearer (_rawtoken token)
      query = [i|${opsEndPoint}/rest-services/published-data/publication/${opsSearchString epodoc}/${opsServiceToEndPoint service}|]
  r <- liftIO $ Wreq.getWith opts query
  let body = r ^. Wreq.responseBody
      xml = XML.parseLBS_ XML.def body
      throttle = parseThrottleStatement $ convertString $ r ^. Wreq.responseHeader "X-Throttling-Control"
  put sessionState {quotaState = throttle}
  return xml

downloadEPODOCPageAsPDF :: OAuth2Token -> EPODOC -> [Char] -> Int -> IO ()
downloadEPODOCPageAsPDF token epodoc path page = do
  let imageLink = rebuildImageLink epodoc
      query = [i|${opsEndPoint}/rest-services/${imageLink}.pdf?Range=${page}|]
      epokey = (convertString $ fromEPODOC epodoc) :: [Char]
      file = printf "%s/%s-%04d.pdf" path epokey page
  printf "[%d] " page
  downloadFile token query file

getEPODOCInstances :: Bool -> EPODOC -> OPSSession [InstanceListing]
getEPODOCInstances strictly epodoc = do
  imagedata <- epoRequest epodoc Images
  let instances = filter allow $ getLinksAndCounts imagedata
      unwantedKind e (c, k) = countryCode e == c && (kind epodoc /= Just k && kind e == Just k)
      allow (l, e)
       | l <= 1 = False
       | strictly && not (e `equivEPODOC` epodoc) = False
       | e `unwantedKind` ("EP", "A3") = False -- exclude search reports, unless we ask for them
       | e `unwantedKind` ("EP", "A4") = False
       | otherwise = True
  when (null instances) $ liftIO $ printf "Nothing found for %s\n" ((convertString . fromEPODOC) epodoc :: [Char])
  return instances

downloadEPODOCInstance :: InstanceListing -> OPSSession ()
downloadEPODOCInstance (count, instanceEPODOC) = do
  token <- authenticate
  let pages = [1..count]
      epokey = formatAsEPODOC instanceEPODOC
      output :: [Char]
      output = [i|${epokey}.pdf|]
  _ <- liftIO $ Temp.withTempDirectory "." "pat-download." $ \tmpDir -> do
        printf "Downloading %s: " epokey
        mapM_ (downloadEPODOCPageAsPDF token instanceEPODOC tmpDir) pages
        Turtle.shell [i|gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=${output} "${tmpDir}"/*.pdf|] Turtle.empty
  liftIO $ printf "Done! \n"

downloadFile :: OAuth2Token -> [Char] -> FilePath -> IO ()
downloadFile token url name = do
  let opts =  Wreq.defaults
            & Wreq.auth ?~ Wreq.oauth2Bearer (_rawtoken token)
  r <- Wreq.getWith opts url
  output <- fromLazyByteString $ r ^. Wreq.responseBody
  S.withFileAsOutput name (S.connect output)

requestOAuthToken :: [Char] -> [Char] -> IO OAuth2Token
requestOAuthToken client_id client_secret = do
    let opts = Wreq.defaults
            & Wreq.header "Accept" .~ ["application/json"]
            & Wreq.header "Content-Type" .~ ["application/x-www-form-urlencoded"]
    let body =
          [ "grant_type" Wreq.:= ("client_credentials" :: [Char])
          , "client_id" Wreq.:= client_id
          , "client_secret" Wreq.:= client_secret]
    resp <- Wreq.postWith opts (opsEndPoint <> "/auth/accesstoken") body
    let token = resp ^. Wreq.responseBody . key "access_token" . _String
    return $ (T.encodeUtf8 >>> OAuth2Token) token

opsSearchString :: EPODOC -> [Char]
opsSearchString epodoc = case kind epodoc of
  Nothing -> [i|epodoc/${formatAsEPODOC epodoc}|]
  Just _  -> [i|docdb/${formatAsDOCDB epodoc}|]

opsServiceToServiceQuota :: OPSService -> OPSServiceQuota
opsServiceToServiceQuota s
  | s == Images = ImagesQuota
  | otherwise = RetrievalQuota

opsServiceToEndPoint :: OPSService -> [Char]
opsServiceToEndPoint s
  | s == Biblio = "biblio"
  | s == Abstract = "abstract"
  | s == FullCycle = "full-cycle"
  | s == FullText = "full-text"
  | s == Description = "description"
  | s == Claims = "claims"
  | s == Equivalents = "equivalents"
  | s == Images = "images"

opsServiceStateFromString :: [Char] -> OPSServiceState
opsServiceStateFromString "idle" = Idle
opsServiceStateFromString "busy" = Busy
opsServiceStateFromString _      = Overloaded

opsServiceTrafficFromString :: [Char] -> OPSServiceTraffic
opsServiceTrafficFromString "green"  = Green
opsServiceTrafficFromString "yellow" = Yellow
opsServiceTrafficFromString "red"    = Red
opsServiceTrafficFromString _        = Black

opsServiceQuotaFromString :: [Char] -> OPSServiceQuota
opsServiceQuotaFromString "retrieval" = RetrievalQuota
opsServiceQuotaFromString "search"    = SearchQuota
opsServiceQuotaFromString "inpadoc"   = INPADOCQuota
opsServiceQuotaFromString "images"    = ImagesQuota
opsServiceQuotaFromString _           = OtherQuota

readDef :: Read a => a -> [Char] -> a
readDef def input = fromMaybe def (readMaybe input)

throttleStatement :: Parsec.Parsec Text () Quotas
throttleStatement = do
  let service_state = do
        service_name <- Parsec.many1 Parsec.letter
        void $ Parsec.char '='
        traffic_light <- Parsec.many1 Parsec.letter
        void $ Parsec.char ':'
        request_limit <- Parsec.many1 Parsec.digit
        return (opsServiceQuotaFromString service_name, (opsServiceTrafficFromString traffic_light, readDef 0 request_limit))
  system_state <- Parsec.choice [Parsec.try $ Parsec.string "idle", Parsec.try $ Parsec.string "busy", Parsec.string "overloaded"]
  void Parsec.spaces
  void $ Parsec.char '('
  services <- Parsec.many1 service_state
  void $ Parsec.char '('
  let service_states = Map.fromList services
  return (opsServiceStateFromString system_state, service_states)

parseThrottleStatement :: Text -> Quotas
parseThrottleStatement input = fromMaybe (Overloaded, Map.empty) $ hush $ Parsec.parse throttleStatement "throttleStatement" input

rebuildImageLink :: EPODOC -> [Char]
rebuildImageLink epodoc = printf "published-data/images/%s/%s/%s/fullimage"
                                  (convertString (countryCode epodoc)::[Char])
                                  (convertString (serial epodoc)::[Char])
                                  (convertString (fromMaybe "%" $ kind epodoc)::[Char])

imageLinktoEPODOC :: Text -> Maybe EPODOC
imageLinktoEPODOC imageLink = hush $ Parsec.parse opsImageFormat "opsImageFormat" imageLink where
  opsImageFormat :: Parsec.Parsec Text () EPODOC
  opsImageFormat = do
    void $ Parsec.string "published-data/images/"
    countryPart <- Parsec.count 2 Parsec.letter
    void $ Parsec.char '/'
    serialPart <- Parsec.many1 Parsec.digit
    void $ Parsec.char '/'
    kindPart <- Parsec.many1 (Parsec.letter <|> Parsec.digit)
    return $ EPODOC (convertString countryPart)
                    (convertString serialPart)
                    (Just (convertString kindPart))
                    Nothing

getLinksAndCounts :: XML.Document -> [InstanceListing]
getLinksAndCounts xml = catMaybes (getLinkAndCount <$> instances)
  where
    cursor = XML.fromDocument xml
    instances = cursor $// XML.laxElement "document-instance" >=> XML.attributeIs "desc" "FullDocument"
    getLinkAndCount instanceCursor =
      let
        instanceEPODOC = imageLinktoEPODOC $ headDef "" (XML.attribute "link" instanceCursor)
        pageCount = join $ (readMaybe . convertString) <$> headMay (XML.attribute "number-of-pages" instanceCursor)
      in case (pageCount, instanceEPODOC) of
          (Just pg, Just iEPODOC) -> Just (pg, iEPODOC)
          (_, _)            -> Nothing
                  
