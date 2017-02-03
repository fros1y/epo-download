{-|
Module      : EPODOC
Description : EPODOC's main module

Parse different citation formats for patent documents into an EPODOC-compliant structure.

http://www.hawkip.com/advice/variations-of-publication-number-formatting-by-country
More here: http://documents.epo.org/projects/babylon/eponet.nsf/0/94AA7EF4AAB18DDEC125806500367F15/$FILE/publ1_20161102_en.pdf

-}
module EPODOC
    ( EPODOC(..),
      parseToEPODOC,
      fromEPODOC,
      equivEPODOC
    ) where

import qualified Data.Char
import           Lib.Prelude
import qualified Text.Parsec as Parsec
import qualified Data.Text as T

data EPODOC = EPODOC {
  countryCode :: Text,
  serial      :: Text,
  kind        :: Maybe Text,
  untrimmedSerial     :: Maybe Text
} deriving (Show, Eq)

equivEPODOC :: EPODOC -> EPODOC -> Bool
equivEPODOC a@(EPODOC{untrimmedSerial=Just u}) b = u `T.isInfixOf` (fromEPODOC b)
equivEPODOC a b@(EPODOC{untrimmedSerial=Just u}) = u `T.isInfixOf` (fromEPODOC a)
equivEPODOC a@(EPODOC {kind=Nothing}) b = (countryCode a) == (countryCode b) && (serial a)== (serial b)
equivEPODOC a b@(EPODOC {kind=Nothing}) = (countryCode a) == (countryCode b) && (serial a)== (serial b)
equivEPODOC a b = a == b

fromEPODOC :: EPODOC -> Text
fromEPODOC epodoc = countryCode epodoc <> serial epodoc <> fromMaybe "" (kind epodoc)

usPubAppFormat :: Parsec.Parsec Text () EPODOC
usPubAppFormat = do
  Parsec.string "US"
  year <- Parsec.count 4 Parsec.digit
  serialNo<- Parsec.count 7 Parsec.digit
  let trimLeadZero = dropWhile (== '0') serialNo -- For some reason, EPO data drops these zeros
      serialPart = year <> trimLeadZero
  kindPart <- Parsec.optionMaybe (Parsec.many1 Parsec.anyChar)
  return $ EPODOC "US"
                (convertString serialPart)
                (convertString <$> kindPart)
                (Just (convertString ("US" <> year <> serialNo)))

epodocFormat :: Parsec.Parsec Text () EPODOC
epodocFormat = do
    countryPart <- Parsec.count 2 Parsec.letter
    serialPart <- Parsec.many1 Parsec.digit
    kindPart <- Parsec.optionMaybe (Parsec.many1 Parsec.anyChar)
    return $ EPODOC (convertString countryPart)
                    (convertString serialPart)
                    (convertString <$> kindPart)
                    Nothing


messyUSPatent :: Parsec.Parsec Text () EPODOC
messyUSPatent = do
  Parsec.optional $ do
    _ <- countryPhrase
    _ <- Parsec.spaces
    _ <- patentPhrase
    _ <- Parsec.spaces
    return ()
  serialNo <- commaSepPatNumber
  return $ EPODOC "US" serialNo Nothing Nothing

lensLikeFormat :: Parsec.Parsec Text () EPODOC
lensLikeFormat = do
  countryPart <- Parsec.count 2 Parsec.letter
  _ <- Parsec.char '_'
  serialPart <- Parsec.many1 Parsec.digit
  _ <- Parsec.char '_'
  kindPart <- Parsec.many1 Parsec.anyChar
  return $ EPODOC (convertString countryPart)
                  (convertString serialPart)
                  (Just (convertString kindPart))
                  Nothing

countryPhrase :: Parsec.Parsec Text () ()
countryPhrase = void $ Parsec.choice [
                  Parsec.try $ Parsec.string "United States",
                  Parsec.try $ Parsec.string "U.S.",
                  Parsec.string "US"]

patentPhrase :: Parsec.Parsec Text () ()
patentPhrase = do
  _ <- typePhrase
  _ <- Parsec.optional $ Parsec.char '.'
  _ <- Parsec.spaces
  _ <- numberSignalPhrase
  _ <- Parsec.optional $ Parsec.char '.'
  _ <- Parsec.spaces
  return ()

typePhrase :: Parsec.ParsecT Text u Identity ()
typePhrase = void $ Parsec.choice [
                Parsec.try $ Parsec.string "Pat",
                Parsec.try $ Parsec.string "Patent"]

numberSignalPhrase :: Parsec.ParsecT Text u Identity ()
numberSignalPhrase = void $ Parsec.choice [
  Parsec.try $ Parsec.string "No",
  Parsec.try $ Parsec.string "Number"]

imperialYear :: Parsec.Parsec Text () Int
imperialYear = foldl' (\a i -> a * 10 + Data.Char.digitToInt i) 0 <$> Parsec.count 2 Parsec.digit

jpxNumber :: Parsec.Parsec Text () EPODOC
jpxNumber = do
  emperor <- Parsec.string "JPS" <|> Parsec.string "JPH"
  year <- imperialYear
  serialPart <- Parsec.count 6 Parsec.digit
  -- http://www.epo.org/searching-for-patents/helpful-resources/asian/japan/numbering.html
  let offset = if emperor == "JPS" then 1925 else 1988
      numbers = convertString $ show (year + offset) ++ serialPart
  return $ EPODOC "JP" numbers (Just "A") (Just (convertString $ emperor<>(show year)<>serialPart)) -- A is the unexamined application.


triplet :: Parsec.ParsecT Text u Identity [Char]
triplet = Parsec.optional (Parsec.char ',') >> Parsec.count 3 Parsec.digit

-- only matching "modern" 7 digit series patents
commaSepPatNumber :: Parsec.Parsec Text () Text
commaSepPatNumber = do
  firstPart <- Parsec.digit
  rest <- Parsec.count 2 triplet
  return $ convertString (firstPart : concat rest)

patentFormats :: Parsec.Parsec Text () EPODOC
patentFormats =  Parsec.choice [Parsec.try usPubAppFormat,
                                Parsec.try epodocFormat,
                                Parsec.try jpxNumber,
                                Parsec.try messyUSPatent,
                                lensLikeFormat]

parseToEPODOC :: Text -> Either Parsec.ParseError EPODOC
parseToEPODOC = Parsec.parse patentFormats "parseToEPODOC"
