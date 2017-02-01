import Protolude
import Test.Hspec

import qualified EPODOC as EPODOC
import qualified EPOOPS as OPS
import Control.Monad.Trans
import Control.Monad.Trans.Either

canonicalUS :: EPODOC.EPODOC
canonicalUS = EPODOC.EPODOC "US" "1234567" Nothing

main :: IO ()
main = hspec $ do
  describe "EPODOC parsing" $ do
    it "fails on bogus input" $
      (EPODOC.parseToEPODOC "Hello world") `shouldSatisfy` isLeft

    it "Preserves string in EPODOC format in round trip" $
      (EPODOC.fromEPODOC <$> (EPODOC.parseToEPODOC "US123456B1")) `shouldBe` (Right "US123456B1")

    it "Groks raw US patent numbers" $
      (EPODOC.parseToEPODOC "1234567") `shouldBe` (Right canonicalUS)

    it "Converts messy patent numbers into EPODOC" $
      (EPODOC.parseToEPODOC "U.S. Pat. No. 1,234,567") `shouldBe` (Right (EPODOC.EPODOC "US" "1234567" Nothing))

    it "Gets US publication numbers right" $
      (EPODOC.parseToEPODOC "US2000123456789") `shouldBe` (Right (EPODOC.EPODOC "US" "2000123456789" Nothing))

  -- describe "OPS queries" $ do
  --   it "fails to get a token on bogus input" $ do
  --     token <- try $ OPS.requestOAuthToken "foo" "bar"
  --     shouldSatisfy token isLeft
  --
  --   it "gets a token on good input" $ do
  --     token <- try $ OPS.requestOAuthToken consumerKey secretKey
  --     shouldSatisfy token isRight
  --
  --   it "pulls biblio data" $ do
  --     token <- OPS.requestOAuthToken consumerKey secretKey
  --     response <- OPS.downloadBiblioJSON token canonicalUS
  --     putStrLn response
