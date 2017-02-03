module Main where

import           Data.String.Conversions (convertString)
import qualified EPODOC                  as EPODOC
import qualified EPOOPS                  as EPOOPS
import           Options
import           Protolude
import           System.IO               (BufferMode (NoBuffering),
                                          hSetBuffering, stdout)

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
    case parse of
      (Left err) -> do
        print err
      (Right epodoc) -> do
        token <- EPOOPS.requestOAuthToken ((convertString . consumerKey) opts) ((convertString . secretKey) opts)
        void $ EPOOPS.downloadEPODDOC token epodoc (strict opts)
