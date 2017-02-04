{-# LANGUAGE QuasiQuotes                #-}

module EPODOC
    ( -- * Data Type
      EPODOC(..),
      -- * Parsing
      parseToEPODOC,
      -- * Stringifying
      formatAsDOCDB,
      formatAsEPODOC,
      -- * Equivalence Testing
      equivEPODOC,
    ) where

import qualified Data.Char
import           Lib.Prelude
import qualified Text.Parsec as Parsec
import qualified Data.Text as T

-- | Store a reference to a patent document in a normalized form
data EPODOC = EPODOC {
  countryCode :: Text, -- ^ Two letter country code
  serial      :: Text, -- ^ Serial number
  kind        :: Maybe Text, -- ^ Possibly a kind code, or an EPODOC that does not specify a particular one.
  untrimmedSerial     :: Maybe Text -- ^ Due to some odd search string issues in the EPO OPS, leading zeros sometimes need to be dropped from the serial number. This maintains the "original" version of it.
} deriving (Show, Eq)

-- | Determines whether two EPODOC are 'equivalent,' here meaning that an EPODOC with a kind code will match one without any kind code, all other fields being equal.
equivEPODOC :: EPODOC -> EPODOC -> Bool
equivEPODOC EPODOC{untrimmedSerial=Just u} b = u `T.isInfixOf` formatAsEPODOC b
equivEPODOC a EPODOC{untrimmedSerial=Just u} = u `T.isInfixOf` formatAsEPODOC a
equivEPODOC a@EPODOC {kind=Nothing} b = countryCode a == countryCode b && serial a == serial b
equivEPODOC a b@EPODOC {kind=Nothing} = countryCode a == countryCode b && serial a == serial b
equivEPODOC a b = a == b

-- | Convert an EPODOC reference into Text that follows the DOCDB formatting convention.
--
-- If no kind code is set in the EPODOC, sets the DOCDB kind code (which is mandatory) to '%', which will sometimes work as a wildcard.
formatAsDOCDB :: EPODOC -> Text
formatAsDOCDB epodoc = convertString (countryCode epodoc <> "." <> serial epodoc <> "." <> fromMaybe "%" (kind epodoc))


-- | Convewrt an EPODOC reference back into EPODOC format.
formatAsEPODOC :: EPODOC -> Text
formatAsEPODOC epodoc = convertString (countryCode epodoc <> serial epodoc <> fromMaybe "" (kind epodoc))

usPubAppFormat :: Parsec.Parsec Text () EPODOC
usPubAppFormat = do
  void $ Parsec.string "US"
  year <- Parsec.count 4 Parsec.digit
  Parsec.optional $ Parsec.char '/'
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
  _ <- Parsec.optional $ numberSignalPhrase
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
imperialYear = foldl' (\a int -> a * 10 + Data.Char.digitToInt int) 0 <$> Parsec.count 2 Parsec.digit

jpxNumber :: Parsec.Parsec Text () EPODOC
jpxNumber = do
  emperor <- Parsec.string "JPS" <|> Parsec.string "JPH"
  year <- imperialYear
  serialPart <- Parsec.count 6 Parsec.digit
  -- http://www.epo.org/searching-for-patents/helpful-resources/asian/japan/numbering.html
  let offset = if emperor == "JPS" then 1925 else 1988
      numbers = convertString $ show (year + offset) ++ serialPart
  return $ EPODOC "JP" numbers (Just "A") (Just (convertString $ emperor <> show year <> serialPart)) -- A is the unexamined application.


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
                                Parsec.try lensLikeFormat]

-- | Parses a variety of textual formats into a normalized EPODOC structure.
--
-- Formats such as US1234567 or EP1234567 are understood, as are messier variations on "U.S. Pat. No. 1,234,567"
-- For some countries, notably JP, a kind code will basically be required to get any results in EPODOC. In other cases,
-- like U.S. patents, it is not required.
--
-- Other formats, like US_1234567_A, or US2016/1234567, are also supported.
-- For more information, check out
-- http://www.hawkip.com/advice/variations-of-publication-number-formatting-by-country
-- http://documents.epo.org/projects/babylon/eponet.nsf/0/94AA7EF4AAB18DDEC125806500367F15/$FILE/publ1_20161102_en.pdf
parseToEPODOC :: Text -> Either Parsec.ParseError EPODOC
parseToEPODOC input = Parsec.parse patentFormats (convertString input) input
