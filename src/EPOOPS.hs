{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : EPOOPS
Description : EPOOPS's main module

-}
module EPOOPS
    ( requestOAuthToken,
    downloadBiblioJSON,
    downloadImageDataJSON,
    downloadEPODDOC
    ) where

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
import qualified Turtle

newtype OAuth2Token = OAuth2Token { _rawtoken :: B.ByteString } deriving (Show, Eq)

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
epoRequest :: OAuth2Token -> Query -> IO Text
epoRequest token query = do
  let opts =  Wreq.defaults
            & Wreq.auth ?~ Wreq.oauth2Bearer (_rawtoken token)
  r <- Wreq.getWith opts query
  let jsonp = (convertString $ r ^. Wreq.responseBody)
      json = T.dropEnd 1 (T.drop 14 jsonp)
  return json

downloadBiblioJSON :: OAuth2Token -> EPODOC -> IO Text
downloadBiblioJSON token epodoc = epoRequest token [i|${opsEndPoint}/rest-services/published-data/publication/epodoc/${epokey}/biblio.js|]
  where
    epokey = (convertString $ fromEPODOC epodoc) :: [Char]

downloadImageDataJSON :: OAuth2Token -> EPODOC -> IO Text
downloadImageDataJSON token epodoc = epoRequest token [i|${opsEndPoint}/rest-services/published-data/publication/epodoc/${epokey}/images.js|]
  where
    epokey = (convertString $ fromEPODOC epodoc) :: [Char]

downloadEPODDOC :: OAuth2Token -> EPODOC -> IO ([Char])
downloadEPODDOC token epodoc = do
  imagedata <- downloadImageDataJSON token epodoc
  let count = fromMaybe 0 (getPageCount imagedata)
      imageLink = getImageLink imagedata
      pages = [1..count]
      epokey = (convertString $ fromEPODOC epodoc) :: [Char]
      output = [i|${epokey}.pdf|]
  _ <- Temp.withTempDirectory "." "pat-download." $ \tmpDir -> do
        printf "Downloading %s: " epokey
        mapM_ (downloadImagePDF tmpDir token epodoc imageLink) pages
        Turtle.shell [i|gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=${output} ./"${tmpDir}"/*.pdf|] Turtle.empty
  printf "Done! \n"
  return output

getImageLink :: Text -> Text
getImageLink json = convertString $ json ^.
  key "ops:world-patent-data" .
  key "ops:document-inquiry" .
  key "ops:inquiry-result" .
  key "ops:document-instance" .
  nth 0 .
  key "@link" . _String

getPageCount :: Text -> Maybe Int
getPageCount json = readMaybe . convertString $ json ^.
      key "ops:world-patent-data" .
      key "ops:document-inquiry" .
      key "ops:inquiry-result" .
      key "ops:document-instance" .
      nth 0 .
      key "@number-of-pages" . _String

downloadImagePDF :: [Char] -> OAuth2Token -> EPODOC -> Text -> Int -> IO ()
downloadImagePDF path token epodoc imageLink range = do
    let query = [i|${opsEndPoint}/rest-services/${imageLink'}.pdf?Range=${range}|]
        imageLink' = (convertString imageLink) :: [Char]
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
