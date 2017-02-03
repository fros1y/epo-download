{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : EPOOPS
Description : EPOOPS's main module

-}
module EPOOPS
    -- ( requestOAuthToken,
    -- downloadBiblioXML,
    -- downloadImageDataXML,
    -- downloadEPODDOC
    -- )
    where

import           Control.Arrow
import           Control.Lens       hiding ((&))
import           Data.Aeson.Lens
import qualified Data.ByteString    as B
import           Data.String.Here
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           EPODOC
import           Lib.Prelude
import qualified Network.Wreq       as Wreq
import           System.IO.Streams  (InputStream, OutputStream,
                                     fromLazyByteString)
import qualified System.IO.Streams  as S
import qualified System.IO.Temp     as Temp
import           Text.Printf        (printf)
import           Text.Read          (readMaybe)
import qualified Turtle hiding ((<>))
import qualified Text.Parsec as Parsec

import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import Text.XML.Cursor (($/), (&/), ($//), (>=>), ($|))

newtype OAuth2Token = OAuth2Token { _rawtoken :: B.ByteString } deriving (Show, Eq)

formatAsDOCDB :: EPODOC -> [Char]
formatAsDOCDB epodoc = [i|${cc}.${serialNo}.${kindCode}|] where
  cc :: [Char]
  cc = (convertString . countryCode) epodoc
  serialNo :: [Char]
  serialNo = (convertString . serial) epodoc
  kindCode :: [Char]
  kindCode = convertString (fromMaybe "%" $ kind epodoc)

formatAsEPODOC :: EPODOC -> [Char]
formatAsEPODOC epodoc = convertString (fromEPODOC epodoc)

opsSearchString :: EPODOC -> [Char]
opsSearchString epodoc = case kind epodoc of
  Nothing -> [i|epodoc/${formatAsEPODOC epodoc}|]
  Just _ -> [i|docdb/${formatAsDOCDB epodoc}|]

opsEndPoint :: [Char]
opsEndPoint = "https://ops.epo.org/3.1"

requestOAuthToken :: Text -> Text -> IO OAuth2Token
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


type Query = [Char]
epoRequest :: OAuth2Token -> Query -> IO XML.Document
epoRequest token query = do
  let opts =  Wreq.defaults
            & Wreq.auth ?~ Wreq.oauth2Bearer (_rawtoken token)
  r <- Wreq.getWith opts query
  let body = r ^. Wreq.responseBody
      xml = XML.parseLBS_ XML.def body
  return xml

downloadBiblioXML :: OAuth2Token -> EPODOC -> IO XML.Document
downloadBiblioXML token epodoc = epoRequest token [i|${opsEndPoint}/rest-services/published-data/publication/${opsSearchString epodoc}/biblio|]

downloadImageDataXML :: OAuth2Token -> EPODOC -> IO XML.Document
downloadImageDataXML token epodoc = epoRequest token [i|${opsEndPoint}/rest-services/published-data/publication/${opsSearchString epodoc}/images|]

rebuildImageLink :: EPODOC -> [Char]
rebuildImageLink epodoc = printf "published-data/images/%s/%s/%s/fullimage"
                                  ((convertString (countryCode epodoc))::[Char])
                                  ((convertString (serial epodoc))::[Char])
                                  ((convertString (fromMaybe "%" $ kind epodoc))::[Char])

imageLinktoEPODOC :: Text -> Maybe EPODOC
imageLinktoEPODOC link = hush $ Parsec.parse opsImageFormat "opsImageFormat" link where
  opsImageFormat :: Parsec.Parsec Text () EPODOC
  opsImageFormat = do
    Parsec.string "published-data/images/"
    countryPart <- Parsec.count 2 Parsec.letter
    Parsec.char '/'
    serialPart <- Parsec.many1 Parsec.digit
    Parsec.char '/'
    kindPart <- Parsec.many1 (Parsec.letter <|> Parsec.digit)
    return $ EPODOC (convertString countryPart)
                    (convertString serialPart)
                    (Just (convertString kindPart))
                    Nothing

type InstanceListing = (Int, EPODOC)

downloadEPODOCInstance :: OAuth2Token -> InstanceListing -> IO ()
downloadEPODOCInstance token (count, instanceEPODOC) = do
  let pages = [1..count]
      epokey = formatAsEPODOC instanceEPODOC
      output :: [Char]
      output = [i|${epokey}.pdf|]
  _ <- Temp.withTempDirectory "." "pat-download." $ \tmpDir -> do
        printf "Downloading %s: " epokey
        mapM_ (downloadImagePDF tmpDir token instanceEPODOC (rebuildImageLink instanceEPODOC)) pages
        Turtle.shell [i|gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=${output} "${tmpDir}"/*.pdf|] Turtle.empty
  printf "Done! \n"



downloadEPODDOC :: OAuth2Token -> EPODOC -> Bool -> IO ([EPODOC])
downloadEPODDOC token rootEPODOC strict = do
  imagedata <- (downloadImageDataXML token rootEPODOC)
  let instances = filter allow $ getLinksAndCounts imagedata
      unwantedKind e (c, k) = if (countryCode e) == c
                          then (kind rootEPODOC) /= Just k && (kind e) == Just k
                          else False
      allow (l, e)
       | l <= 1 = False
       | strict && not (e `equivEPODOC` rootEPODOC) = False
       | e `unwantedKind` ("EP", "A3") = False -- exclude search reports, unless we ask for them
       | e `unwantedKind` ("EP", "A4") = False
       | otherwise = True
  mapM_ (downloadEPODOCInstance token) instances
  return $ map snd instances



getLinksAndCounts :: XML.Document -> [InstanceListing]
getLinksAndCounts xml = catMaybes (getLinkAndCount <$> instances)
  where
    cursor = XML.fromDocument xml
    instances = cursor $// XML.laxElement "document-instance" >=> XML.attributeIs "desc" "FullDocument"

getLinkAndCount :: XML.Cursor -> Maybe InstanceListing
getLinkAndCount cursor = listing
  where
    listing = case (pageCount, instanceEPODOC) of
        (Just pg, Just i) -> Just (pg, i)
        (_, _) -> Nothing
    instanceEPODOC = imageLinktoEPODOC $ headDef "" (XML.attribute "link" cursor)
    pageCount = join $ (readMaybe . convertString) <$> headMay (XML.attribute "number-of-pages" cursor)

downloadImagePDF :: [Char] -> OAuth2Token -> EPODOC -> [Char] -> Int -> IO ()
downloadImagePDF path token epodoc imageLink range = do
    let query = [i|${opsEndPoint}/rest-services/${imageLink}.pdf?Range=${range}|]
        epokey = (convertString $ fromEPODOC epodoc) :: [Char]
        file = printf "%s/%s-%04d.pdf" path epokey range --[i|download-${range}.pdf|]
    printf "[%d] " range
    downloadFile token query file

withProgressBar :: Integer
                -> InputStream ByteString
                -> OutputStream ByteString
                -> IO ()
withProgressBar fileSize inS outS = go (0 :: Int)
  where
    go blocksRead = do
      block <- S.read inS
      case block of
        (Just d) -> do
            let currentBlocks = blocksRead + B.length d
            let percentage = fromIntegral (currentBlocks * 100) /
                             fromIntegral fileSize
            -- printf "%10d [%3.2f%%]\r" currentBlocks (percentage :: Double)
            S.write (Just d) outS >> go currentBlocks
        Nothing -> return ()

downloadFile :: OAuth2Token -> [Char] -> FilePath -> IO ()
downloadFile token url name = do
  let opts =  Wreq.defaults
            & Wreq.auth ?~ Wreq.oauth2Bearer (_rawtoken token)
  r <- Wreq.getWith opts url
  let fileSize = fromMaybe 0 . readMaybe . convertString $ r ^. Wreq.responseHeader "content-length"
  output <- fromLazyByteString $ r ^. Wreq.responseBody
  S.withFileAsOutput name (withProgressBar fileSize output)
