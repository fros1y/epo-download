module Main where

import           Data.String.Conversions (convertString)
import qualified EPODOC                  as EPODOC
import qualified EPOOPS                  as EPOOPS
import           Options
import           Protolude
import           System.IO               (BufferMode (NoBuffering),
                                          hSetBuffering, stdout)
import Network.HTTP.Types.Status
import Network.HTTP.Types
import Network.HTTP.Client

data PatentOptions = PatentOptions
  {
    consumerKey :: [Char],
    secretKey   :: [Char],
    strict      :: Bool
  }

instance Options PatentOptions where
  defineOptions = pure PatentOptions
    <*> simpleOption "consumerKey" ""
        "Consumer Key from EPO OPS"
    <*> simpleOption "secretKey" ""
        "Secret Key from EPO OPS"
    <*> simpleOption "strict" True
        "Limit retrived documents to specific EPODOC input"

main :: IO ()
main = runCommand $ \opts args -> do
    hSetBuffering stdout NoBuffering
    let parse = EPODOC.parseToEPODOC . convertString $ headDef "" args
        onNotFoundError (StatusCodeException s _ _)
          | (statusCode s) == 404 = Just ("Not Found" :: [Char])
          | otherwise = Nothing
        onNotFoundError _ = Nothing
    case parse of
      (Left err) -> do
        print err
      (Right epodoc) -> handleJust onNotFoundError (putStrLn) $ do
        token <- EPOOPS.requestOAuthToken ((convertString . consumerKey) opts) ((convertString . secretKey) opts)
        void $ EPOOPS.downloadEPODDOC token epodoc (strict opts)
