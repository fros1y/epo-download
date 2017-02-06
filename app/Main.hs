module Main where

import           Data.String.Conversions (convertString)
import qualified EPODOC                  as EPODOC
import qualified EPOOPS                  as EPOOPS
import           Options
import           Protolude
import           System.IO               (BufferMode (NoBuffering),
                                          hSetBuffering, stdout)
import Network.HTTP.Types.Status
import Network.HTTP.Client
import           Text.Printf        (printf)

data PatentOptions = PatentOptions
  {
    consumerKey :: [Char],
    secretKey   :: [Char],
    strict      :: Bool,
    debug       :: Bool,
    saveText    :: Bool,
    savePDF     :: Bool
  }

instance Options PatentOptions where
  defineOptions = pure PatentOptions
    <*> simpleOption "consumerKey" ""
        "Consumer Key from EPO OPS"
    <*> simpleOption "secretKey" ""
        "Secret Key from EPO OPS"
    <*> simpleOption "strict" True
        "Limit retrived documents to specific EPODOC input"
    <*> simpleOption "debug" False
        "Display debugging messages"
    <*> simpleOption "saveText" True
        "Attempt to save a text version of EPODOC"
    <*> simpleOption "savePDF" True
        "Download EPODOC as PDF"

pageProgress :: EPOOPS.PageProgress
pageProgress total curr = printf "[%i/%i] " curr total

perInstance :: Bool -> Bool -> EPOOPS.InstanceListing -> EPOOPS.OPSSession ()
perInstance saveP saveT epodocInstance = do
  let epodoc = EPODOC.formatAsEPODOC $ snd epodocInstance
  liftIO $ printf "Downloading %s: " epodoc
  when saveP $ EPOOPS.downloadEPODOCInstance pageProgress epodocInstance
  when saveT $ do
    fullText <- EPOOPS.getEPODOCFullPlainText "EN" (snd epodocInstance)
    case fullText of
      Just t -> liftIO $ do
        writeFile (printf "%s.txt" epodoc) t
        putStrLn ("[Text]" :: [Char])
      Nothing -> liftIO $ printf "Error downloading text for %s\n" epodoc

main :: IO ()
main = runCommand $ \opts args -> do
    hSetBuffering stdout NoBuffering
    when (length args == 0) $ die "You must enter at least one patent document number.\n"
    let parse = EPODOC.parseToEPODOC . convertString $ headDef "" args
        onNotFoundError epodoc (StatusCodeException s _ _)
          | (statusCode s) == 404 = Just (("EPODOC " <> (EPODOC.formatAsEPODOC epodoc) <> " was not found."))
          | otherwise = Nothing
        onNotFoundError _ _ = Nothing
        credentials = EPOOPS.Credentials (consumerKey opts) (secretKey opts)
        logLevel = if debug opts then EPOOPS.LevelDebug else EPOOPS.LevelWarn
    case parse of
      (Left err) -> do
        die $ printf "Input format error: %s\n" (show err :: [Char])
      (Right epodoc) ->
        handleJust (onNotFoundError epodoc) (die . convertString) $
          void $ EPOOPS.withOPSSession credentials logLevel $ do
            instances <- EPOOPS.getEPODOCInstances (strict opts) epodoc
            forM_ instances (perInstance (savePDF opts) (saveText opts))
