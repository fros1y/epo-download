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
    secretKey   :: [Char]
  }

instance Options PatentOptions where
  defineOptions = pure PatentOptions
    <*> simpleOption "consumerKey" ""
        "Consumer Key from EPO OPS"
    <*> simpleOption "secretKey" ""
        "Secret Key from EPO OPS"

main :: IO ()
main = runCommand $ \opts args -> do
    hSetBuffering stdout NoBuffering
    let epodocList = rights (map (EPODOC.parseToEPODOC . convertString) args)
    token <- EPOOPS.requestOAuthToken ((convertString . consumerKey) opts) ((convertString . secretKey) opts)
    mapM_ (EPOOPS.downloadEPODDOC token) epodocList
