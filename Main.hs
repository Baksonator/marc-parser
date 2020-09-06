{-# LANGUAGE OverloadedStrings, DeriveGeneric, EmptyCase #-}

module Main where

import Text.Parsec
import System.Environment
import Data.Char
import System.IO
import qualified Data.Configurator as C
import Data.Configurator.Types
import qualified Data.HashMap.Strict as M
import Data.Text.Internal
import qualified Data.Text as T
import Data.Maybe
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as P
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.String.Builder
import Data.Either
import Data.String
import qualified Data.SortedList as S


-- Grammar
data Doc = Doc { records :: [Marc], docFormat :: Maybe Format} deriving Generic

data Marc = Marc { leader :: Leader, controlfields :: [ControlField], datafields :: [DataField], marcFormat :: Maybe Format } deriving Generic 

data Leader = Leader { leaderTag :: Maybe LeaderTag, leaderValue :: LeaderValue, leaderFormat :: Maybe Format } deriving Generic

data ControlField = ControlField { cfNumberTag :: NumberTag, controlValue :: ControlValue, cfFormat :: Maybe Format } deriving (Generic, Eq)

data DataField = DataField { dfNumberTag :: NumberTag, ind1 :: Indicator, ind2 :: Indicator, subfields :: [SubField], dfFormat :: Maybe Format } deriving (Generic, Eq)

data SubField = SubField { fieldTag :: FieldTag, fieldValue :: FieldValue, sfFormat :: Maybe Format } deriving (Generic, Eq)

data MarcJSON = MarcJSON { leader1 :: String, controlfieldsJSON :: [ControlFieldJSON], datafieldsJSON :: [DataFieldJSON] } deriving (Generic, Show)

data ControlFieldJSON = ControlFieldJSON { controlValueJSON :: String } deriving (Generic, Show)

data DataFieldJSON = DataFieldJSON { dfNumberTagJSON :: DataFieldJSON1 } deriving (Generic, Show)

data DataFieldJSON1 = DataFieldJSON1 { ind1JSON :: String, ind2JSON :: String, subfieldsJSON :: [SubFieldJSON] } deriving (Generic, Show)

data SubFieldJSON = SubFieldJSON { fieldValueJSON :: String } deriving (Generic, Show)


-- Type synonyms
type LeaderTag = String
type LeaderValue = String
type NumberTag = String
type ControlValue = String
type Indicator = String
type FieldTag = String
type FieldValue = String
type NumTabs = Int
type Format = String


-- Deriving the Show typeclass for writing in JSON format
instance Show Doc where
    show (Doc records format)
      | format == Just "json" = "[\n" ++ showRecordsJSON records ++ "]"
      | format == Just "xml" = "<collection>\n" ++ showRecordsXML records ++ "</collection>"
      | format == Just "yaml" = showRecordsYAML records
      | otherwise = "[\n" ++ showRecordsJSON records ++ "]"
        where showRecordsJSON [] = ""
              showRecordsJSON (x:xs)
                | null xs = show x ++ "\n"
                | otherwise = show x ++ ",\n" ++ showRecordsJSON xs
              showRecordsXML [] = ""
              showRecordsXML (x:xs)
                | null xs = show x ++ "\n"
                | otherwise = show x ++ "\n" ++ showRecordsXML xs
              showRecordsYAML [] = ""
              showRecordsYAML (x:xs)
                | null xs = show x ++ "\n"
                | otherwise = show x ++ "\n" ++ showRecordsYAML xs

instance Show Marc where
    show (Marc ldr cntrl dataFlds format)
      | format == Just "json" = "\t{\n" ++ show ldr ++ ",\n\t\t\"controlfields\":\n\t\t[" ++ showCntrlJSON cntrl ++ "\n\t\t],\n\t\t\"datafields\":\n\t\t[" ++ showDataJSON dataFlds ++ "\n\t\t]\n\t}"
      | format == Just "xml" = "  <record>\n" ++ show ldr ++ "\n" ++ showCntrlXML cntrl ++ showDataXML dataFlds ++ "  </record>"
      | format == Just "yaml" = "-\n" ++ show ldr ++ "\n" ++ "  controlfields:\n" ++ showCntrlYAML cntrl ++ "  datafields:\n" ++ showDataYAML dataFlds
      | otherwise = "\t{\n" ++ show ldr ++ ",\n\t\t\"controlfields\":\n\t\t[" ++ showCntrlJSON cntrl ++ "\n\t\t],\n\t\t\"datafields\":\n\t\t[" ++ showDataJSON dataFlds ++ "\n\t\t]\n\t}"
        where showCntrlJSON [] = ""
              showCntrlJSON (x:xs)
                | null xs = if null dataFlds then "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t}" else "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t}"
                | otherwise = "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t}," ++ showCntrlJSON xs
              showDataJSON [] = ""
              showDataJSON (x:xs)
                | null xs = "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t}"
                | otherwise = "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t}," ++ showDataJSON xs
              showCntrlXML [] = ""
              showCntrlXML (x:xs)
                | null xs = show x ++ "\n"
                | otherwise = show x ++ "\n" ++ showCntrlXML xs
              showDataXML [] = ""
              showDataXML (x:xs)
                | null xs = show x ++ "\n"
                | otherwise = show x ++ "\n" ++ showDataXML xs
              showCntrlYAML [] = ""
              showCntrlYAML (x:xs)
                | null xs = "    -\n" ++ show x ++ "\n"
                | otherwise = "    -\n" ++ show x ++ "\n" ++ showCntrlYAML xs
              showDataYAML [] = ""
              showDataYAML (x:xs)
                | null xs = show x ++ "\n"
                | otherwise = show x ++ "\n" ++ showDataYAML xs
                          
instance Show Leader where
    show (Leader tag value format)
      | format == Just "json" = "\t\t\"leader\":" ++ show value
      | format == Just "xml" = "    <leader>" ++ id value ++ "</leader>"
      | format == Just "yaml" = "  leader: " ++ id value
      | otherwise = "\t\t\"leader\":" ++ show value
    
instance Show ControlField where
    show (ControlField tag value format)
      | format == Just "json" = "\t\t\t\t" ++ show tag ++ ":" ++  show value
      | format == Just "xml" = "    <controlfield tag=" ++ show tag ++ ">" ++ id value ++ "</controlfield>"
      | format == Just "yaml" = "      " ++ id tag ++ ": " ++ id value
      | otherwise = "\t\t\t\t" ++ show tag ++ ":" ++  show value
   
instance Show DataField where
    show (DataField tag ind1 ind2 subFlds format)
      | format == Just "json" = "\t\t\t\t" ++ show tag ++ ":\n\t\t\t\t{\n\t\t\t\t\t\"ind1\":" ++ show ind1 ++ ",\n\t\t\t\t\t\"ind2\":" ++ show ind2 ++ ",\n\t\t\t\t\t\"subfields\":\n\t\t\t\t\t[" ++ showSubFldsJSON subFlds ++ "\n\t\t\t\t\t]\n\t\t\t\t}"
      | format == Just "xml" = "    <datafield tag=" ++ show tag ++ " ind1=" ++ show ind1 ++ " ind2=" ++ show ind2 ++ ">\n" ++ showSubFldsXML subFlds ++ "    </datafield>"
      | format == Just "yaml" = "    -\n" ++ "      " ++ id tag ++ ":\n" ++ "        -\n          ind1: " ++ id ind1 ++ "\n        -\n          ind2: " ++ id ind2 ++ "\n        -\n          subfields:\n" ++ showSubFldsYAML subFlds
      | otherwise = "\t\t\t\t" ++ show tag ++ ":\n\t\t\t\t{\n\t\t\t\t\t\"ind1\":" ++ show ind1 ++ ",\n\t\t\t\t\t\"ind2\":" ++ show ind2 ++ ",\n\t\t\t\t\t\"subfields\":\n\t\t\t\t\t[" ++ showSubFldsJSON subFlds ++ "\n\t\t\t\t\t]\n\t\t\t\t}"
        where showSubFldsJSON [] = ""
              showSubFldsJSON (x:xs)
                | null xs = "\n\t\t\t\t\t\t{\n" ++ show x ++ "\n\t\t\t\t\t\t}"
                | otherwise = "\n\t\t\t\t\t\t{\n" ++ show x ++ "\n\t\t\t\t\t\t}," ++ showSubFldsJSON xs
              showSubFldsXML [] = ""
              showSubFldsXML (x:xs)
                | null xs = show x ++ "\n"
                | otherwise = show x ++ "\n" ++ showSubFldsXML xs
              showSubFldsYAML [] = ""
              showSubFldsYAML (x:xs)
                | null xs = show x ++ "\n"
                | otherwise = show x ++ "\n" ++ showSubFldsYAML xs
                          
instance Show SubField where
    show (SubField tag value format)
      | format == Just "json" = "\t\t\t\t\t\t\t" ++ show tag ++ ":" ++ show value
      | format == Just "xml" = "      <subfield code=" ++ show tag ++ ">" ++ id value ++ "</subfield>"
      | format == Just "yaml" = "            " ++ id tag ++ ": " ++ id value
      | otherwise = "\t\t\t\t\t\t\t" ++ show tag ++ ":" ++ show value


instance A.FromJSON Doc where
  parseJSON (A.Object v) = 
    Doc <$> (A..:) v "records"
        <*> (A..:?) v "docFormat"

instance A.FromJSON Marc where
  parseJSON (A.Object v) = 
    Marc <$> v A..: "leader"
         <*> v A..: "controlfields"
         <*> v A..: "datafields"
         <*> v A..:? "marcFormat"

instance A.FromJSON Leader where
  parseJSON (A.Object v) =
    Leader <$> v A..:? "leaderTag"
           <*> v A..: "leaderValue"
           <*> v A..:? "leaderFormat"

instance A.FromJSON ControlField where
  parseJSON (A.Object v) = 
    ControlField <$> v A..: "cfNumberTag"
                 <*> v A..: "controlValue"
                 <*> v A..:? "cfFormat"

instance A.FromJSON DataField where
  parseJSON (A.Object v) =
    DataField <$> v A..: "dfNumberTag"
              <*> v A..: "ind1"
              <*> v A..: "ind2"
              <*> v A..: "subfields"
              <*> v A..:? "dfFormat"

instance A.FromJSON SubField where
  parseJSON (A.Object v) =
    SubField <$> v A..: "fieldTag"
             <*> v A..: "fieldValue"
             <*> v A..:? "sfFormat"

instance A.ToJSON Doc where
  toJSON doc = case A.genericToJSON A.defaultOptions doc of
    A.Object o -> A.Object (M.delete (fromString "docFormat") o)
    _ -> error "impossible"

instance A.ToJSON Marc where
  toJSON marc = case A.genericToJSON A.defaultOptions marc of
    A.Object o -> A.Object (M.delete (fromString "marcFormat") o)
    _ -> error "impossible"

instance A.ToJSON Leader where
  toJSON leader = case A.genericToJSON A.defaultOptions leader of
    A.Object o -> A.Object (M.delete (fromString "leaderTag") (M.delete (fromString "leaderFormat") o))
    _ -> error "impossible"

instance A.ToJSON ControlField where 
  toJSON controlfield = case A.genericToJSON A.defaultOptions controlfield of
    A.Object o -> A.Object (M.delete (fromString "cfFormat") o)
    _ -> error "impossible"

instance A.ToJSON DataField where
  toJSON datafield = case A.genericToJSON A.defaultOptions datafield of
    A.Object o -> A.Object (M.delete (fromString "dfFormat") o)
    _ -> error "impossible"

instance A.ToJSON SubField where
  toJSON subfield = case A.genericToJSON A.defaultOptions subfield of
    A.Object o -> A.Object (M.delete (fromString "sfFormat") o)
    _ -> error "impossible"

-- instance Eq ControlField
-- instance Eq DataField
-- instance Eq SubField

instance Ord ControlField where
  compare (ControlField tag1 _ _) (ControlField tag2 _ _ ) = compare tag1 tag2

instance Ord DataField where
  compare (DataField tag1 _ _ _ _) (DataField tag2 _ _ _ _) = compare tag1 tag2

instance Ord SubField where
  compare (SubField tag1 _ _) (SubField tag2 _ _) = compare tag1 tag2

instance A.FromJSON MarcJSON
instance A.FromJSON ControlFieldJSON
instance A.FromJSON DataFieldJSON
instance A.FromJSON DataFieldJSON1
instance A.FromJSON SubFieldJSON

    
charToString :: Char -> String
charToString c = [c]


stringToChar :: String -> Char
stringToChar s = head s
    
    
-- Parser
document :: M.HashMap Name Value -> Format -> Parsec String Int Doc
document hashMap format = do spaces
--                             res <- many (try (customFormat hashMap format))
--                             res <- try alephSeq
                             res <- many (try (alephSeq format) <|> try (marc21 format) <|> try (marc21congress format) <|> try (marc21leader format) <|> try (unimarc format) <|> try (unimarcsame format) <|> try (customFormat hashMap format))
                             spaces
                             eof
                             return (Doc res (Just format))

              
-- Marc21              
marc21 :: Format -> Parsec String Int Marc
marc21 format = do spaces
                   ldr <- leader21 format
                   spaces
                   cntrl <- many (try (cntFld21 format))
                   spaces
                   dataFlds <- many (try (dataFld21 format))
                   spaces
                   return (Marc ldr cntrl dataFlds (Just format))
            
leader21 :: Format -> Parsec String Int Leader
leader21 format = do tag <- string "LDR"
                     spaces
                     value <- many (noneOf "\n")
                     return (Leader (Just tag) value (Just format))
              
cntFld21 :: Format -> Parsec String Int ControlField
cntFld21 format = do tag <- count 3 digit
                     space
                     space
                     value <- many (noneOf "\n")
                     spaces
                     return (ControlField tag value (Just format))
              
dataFld21 :: Format -> Parsec String Int DataField
dataFld21 format = do tag <- count 3 digit
                      spaces
                      ind1 <- string "#" <|> (count 1 digit)
                      ind2 <- string "#" <|> (count 1 digit)
                      spaces
                      subFlds <- many1 (subFld21 format)
                      return (DataField tag ind1 ind2 subFlds (Just format))

subFld21 :: Format -> Parsec String Int SubField
subFld21 format = do string "$"
                     tag <- (count 1 alphaNum)
                     spaces
                     value <- many (noneOf "$\n")
                     spaces
                     return (SubField tag value (Just format))
             
             
-- Marc21 - Library of Congress            
marc21congress :: Format -> Parsec String Int Marc
marc21congress format = do spaces
                           ldr <- leader21congress format
                           spaces
                           cntrl <- many (try (cntFld21congress format))
                           spaces
                           dataFlds <- many (try (dataFld21congress format))
                           spaces
                           return (Marc ldr cntrl dataFlds (Just format))

leader21congress :: Format -> Parsec String Int Leader
leader21congress format = do tag <- string "000"
                             spaces
                             value <- many (noneOf "\n")
                             return (Leader (Just tag) value (Just format))

cntFld21congress :: Format -> Parsec String Int ControlField
cntFld21congress format = do tag <- count 3 digit
                             spaces
                             lookAhead (do count 2 anyChar
                                           spaces
                                           notFollowedBy (char '|'))
                             lookAhead (do count 2 anyChar
                                           spaces
                                           notFollowedBy (char '['))
                             value <- many (noneOf "\n")
                             spaces
                             return (ControlField tag value (Just format))

dataFld21congress :: Format -> Parsec String Int DataField
dataFld21congress format = do tag <- count 3 digit
                              spaces
                              ind1 <- string "_" <|> (count 1 digit)
                              ind2 <- string "_" <|> (count 1 digit)
                              spaces
                              subFlds <- many1 (subFld21congress format)
                              return (DataField tag ind1 ind2 subFlds (Just format))

subFld21congress :: Format -> Parsec String Int SubField
subFld21congress format = do string "|"
                             tag <- (count 1 alphaNum)
                             spaces
                             value <- sepBy (many (noneOf " |\n")) (string " ")
                             spaces
                             return (SubField tag (listToString value) (Just format))
            
            
-- Marc21 with LEADER                    
marc21leader :: Format -> Parsec String Int Marc
marc21leader format = do spaces
                         ldr <- leader21leader format
                         spaces
                         cntrl <- many (try (cntFld21leader format))
                         spaces
                         dataFlds <- many (try (dataFld21leader format))
                         spaces
                         return (Marc ldr cntrl dataFlds (Just format))

leader21leader :: Format -> Parsec String Int Leader
leader21leader format = do tag <- string "LEADER"
                           spaces
                           value <- many (noneOf "\n")
                           return (Leader (Just tag) value (Just format))

cntFld21leader :: Format -> Parsec String Int ControlField
cntFld21leader format = do tag <- count 3 digit
                           spaces
                           lookAhead (do count 2 (noneOf "$")
                                         spaces
                                         noneOf "$")
                           value <- many (noneOf "\n")
                           spaces
                           return (ControlField tag value (Just format))                    

dataFld21leader :: Format -> Parsec String Int DataField
dataFld21leader format = do tag <- count 3 digit
                            spaces
                            ind1 <- option ("") (try (count 1 digit))
                            spaces
                            ind2 <- option ("") (try (count 1 digit))
                            spaces
                            subFlds <- many1 (subFld21leader format)
                            return (DataField tag ind1 ind2 subFlds (Just format))

subFld21leader :: Format -> Parsec String Int SubField
subFld21leader format = do string "$"
                           tag <- (count 1 alphaNum)
                           spaces
                           value <- sepBy (many (noneOf " $\n")) (string " ")
                           spaces
                           return (SubField tag (listToString value) (Just format))
      
      
-- UNIMARC                    
unimarc :: Format -> Parsec String Int Marc
unimarc format = do spaces
                    let ldr = (Leader (Just "") "" (Just format))
                    spaces
                    unildr <- unimarcleader format
                    spaces
                    dataFlds <- many1 (try (dataFldunimarc format))
                    spaces
                    return (Marc ldr [] (unildr:dataFlds) (Just format))
 
unimarcleader :: Format -> Parsec String Int DataField
unimarcleader format = do tag <- string "001"
                          spaces
                          ind1 <- string "#" <|> (count 1 digit)
                          ind2 <- string "#" <|> (count 1 digit)
                          spaces
                          subFlds <- many (subFldunimarc format)
                          return (DataField tag ind1 ind2 subFlds (Just format)) 

dataFldunimarc :: Format -> Parsec String Int DataField
dataFldunimarc format = do lookAhead (do spaces
                                         notFollowedBy (string "001"))
                           tag <- count 3 digit
                           spaces
                           ind1 <- string "#" <|> (count 1 digit)
                           ind2 <- string "#" <|> (count 1 digit)
                           spaces
                           subFlds <- many1 (subFldunimarc format)
                           return (DataField tag ind1 ind2 subFlds (Just format))

subFldunimarc :: Format -> Parsec String Int SubField
subFldunimarc format = do char '['
                          tag <- (count 1 alphaNum)
                          char ']'
                          value <- many (noneOf "[\n")
                          spaces
                          return (SubField tag value (Just format))
        
        
-- UNIMARC inline             
unimarcsame :: Format -> Parsec String Int Marc
unimarcsame format = do spaces
                        let ldr = (Leader (Just "") "" (Just format))
                        spaces
                        dataFlds <- many1 (try (dataFldunimarcsame format))
                        return (Marc ldr [] dataFlds (Just format))                                                                                                      
                    
dataFldunimarcsame :: Format -> Parsec String Int DataField
dataFldunimarcsame format = do tag <- count 3 digit
                               ind1 <- count 1 (digit <|> space)
                               ind2 <- count 1 (digit <|> space)
                               subFlds <- many (subFldunimarcsame format)
                               many (char (toEnum 30))
                               return (DataField tag ind1 ind2 subFlds (Just format))                                        
                   
subFldunimarcsame :: Format -> Parsec String Int SubField
subFldunimarcsame format = do char (toEnum 31)
                              tag <- (count 1 alphaNum)
                              value <- many (noneOf ([toEnum 31]++[toEnum 30]++"\n"))
                              return (SubField tag value (Just format))


-- Aleph Sequential
alephSeq :: Format -> Parsec String Int Marc
alephSeq format = do spaces
                     ldr <- ldrAlephSeq format
                     spaces
                     cntrl <- many (try (cntFldAlephSeq format))
                     spaces
                     dataFlds <- many (try (dataFldAlephSeq format))
                     spaces
                     return (Marc ldr cntrl dataFlds (Just format))

ldrAlephSeq :: Format -> Parsec String Int Leader
ldrAlephSeq format = do count 9 digit
                        space
                        tag <- string "LDR"
                        spaces
                        char 'L'
                        spaces
                        value <- many (noneOf "\n")
                        return (Leader (Just tag) value (Just format))

cntFldAlephSeq :: Format -> Parsec String Int ControlField
cntFldAlephSeq format = do count 9 digit
                           space
                           tag <- count 3 digit
                           spaces
                           char 'L'
                           spaces
                           lookAhead (do notFollowedBy (char '$'))
                           value <- many (noneOf "\n")
                           spaces
                           return (ControlField tag value (Just format))

dataFldAlephSeq :: Format -> Parsec String Int DataField
dataFldAlephSeq format = do count 9 digit
                            space
                            tag <- count 3 digit
                            ind1 <- option ("#") (try (count 1 digit))
                            spaces
                            ind2 <- option ("#") (try (count 1 digit))
                            spaces
                            char 'L'
                            spaces
                            subFlds <- many1 (subFldAlephSeq format)
                            return (DataField tag ind1 ind2 subFlds (Just format))

subFldAlephSeq :: Format -> Parsec String Int SubField
subFldAlephSeq format = do string "$$"
                           spaces
                           tag <- (count 1 alphaNum)
                           spaces
                           value <- many (noneOf "$\n")
                           spaces
                           return (SubField tag value (Just format))


-- Custom format
customFormat :: M.HashMap Name Value -> Format -> Parsec String Int Marc
customFormat hashMap format = do spaces
                                 let a = fromJust (convert (fromJust (M.lookup "leader-exists" hashMap)))
                                 let b = if a then (customLeader hashMap format) else (return (Leader (Just "") "" (Just format)))
                                 ldr <- b
                                 spaces
                                 cntrl <- many (try (customControlField hashMap format))
                                 spaces
                                 dataFlds <- many1 (try (customDataField hashMap format))
                                 return (Marc ldr cntrl dataFlds (Just format))

customLeader :: M.HashMap Name Value -> Format -> Parsec String Int Leader
customLeader hashMap format = do spaces
                                 let ldrLabel = fromJust (convert (fromJust (M.lookup "leader-label" hashMap)))
                                 let ldrTagPrefixLen = fromJust (convert (fromJust (M.lookup "leader-tag-prefix-len" hashMap)))
                                 let ldrValuePrefixLen = fromJust (convert (fromJust (M.lookup "leader-value-prefix-len" hashMap)))
                                 count ldrTagPrefixLen anyChar
                                 spaces
                                 tag <- string ldrLabel
                                 spaces
                                 count ldrValuePrefixLen anyChar
                                 spaces
                                 value <- many (noneOf "\n")
                                 return (Leader (Just tag) value (Just format))

customControlField :: M.HashMap Name Value -> Format -> Parsec String Int ControlField
customControlField hashMap format = do let indSymbol = fromJust (convert (fromJust (M.lookup "indicator-non-exists-symbol" hashMap)))
                                       let subFldSymbol = fromJust (convert (fromJust (M.lookup "subfield-symbol" hashMap)))
                                       let cntFldTagPrefixLen = fromJust (convert (fromJust (M.lookup "control-field-tag-prefix-len" hashMap)))
                                       let cntFldValuePrefixLen = fromJust (convert (fromJust (M.lookup "control-field-value-prefix-len" hashMap)))
                                       spaces
                                       count cntFldTagPrefixLen anyChar
                                       spaces
                                       tag <- count 3 digit
                                       spaces
                                       count cntFldValuePrefixLen anyChar
                                       spaces
                                       lookAhead (do notFollowedBy (char (stringToChar indSymbol)))
                                       lookAhead (do notFollowedBy (char (stringToChar subFldSymbol)))
                                       --lookAhead (do count 2 anyChar
                                       --              spaces
                                       --              notFollowedBy (char (stringToChar subFldSymbol)))
                                       value <- many (noneOf "\n")
                                       spaces
                                       return (ControlField tag value (Just format))

customDataField :: M.HashMap Name Value -> Format -> Parsec String Int DataField
customDataField hashMap format = do let a = fromJust (convert (fromJust (M.lookup "indicator-needed" hashMap)))
                                    let indSymbol = fromJust (convert (fromJust (M.lookup "indicator-non-exists-symbol" hashMap)))
                                    let dataFldTagPrefixLen = fromJust (convert (fromJust (M.lookup "data-field-tag-prefix-len" hashMap)))
                                    let dataFldValuePrefixLen = fromJust (convert (fromJust (M.lookup "data-field-value-prefix-len" hashMap)))
                                    spaces
                                    count dataFldTagPrefixLen anyChar
                                    spaces
                                    tag <- count 3 digit
                                    spaces
                                    let ind1Help = if a then (string indSymbol <|> (count 1 digit)) else (option ("#") (try (count 1 digit)))
                                    let ind2Help = if a then (string indSymbol <|> (count 1 digit)) else (option ("#") (try (count 1 digit)))
                                    ind1 <- ind1Help
                                    ind2 <- ind2Help
                                    spaces
                                    count dataFldValuePrefixLen anyChar
                                    spaces
                                    subFlds <- many1 (customSubfield hashMap format)
                                    return (DataField tag ind1 ind2 subFlds (Just format))

customSubfield :: M.HashMap Name Value -> Format -> Parsec String Int SubField
customSubfield hashMap format = do let subFldSymbol = fromJust (convert (fromJust (M.lookup "subfield-symbol" hashMap)))
                                   let subFldSymbolEndExists = fromJust (convert (fromJust (M.lookup "subfield-end-exists" hashMap)))
                                   let subFldSymbolEnd = fromJust (convert (fromJust (M.lookup "subfield-end-symbol" hashMap)))
                                   string subFldSymbol
                                   tag <- (count 1 alphaNum)
                                   if subFldSymbolEndExists then (do string subFldSymbolEnd
                                                                     spaces) else spaces
                                   spaces
                                   value <- many (noneOf (subFldSymbol ++ "\n"))
                                   spaces
                                   return (SubField tag value (Just format))


reverseCustomDoc :: M.HashMap Name Value -> Doc -> String
reverseCustomDoc hashMap (Doc [] format) = ""
reverseCustomDoc hashMap (Doc (x:xs) format) = (reverseCustomFormat hashMap x) ++ (reverseCustomDoc hashMap (Doc xs format))

reverseCustomFormat :: M.HashMap Name Value -> Marc -> String
reverseCustomFormat hashMap marc
  | leaderExists == True = (reverseCustomLeader hashMap (leader marc)) ++ "\n" ++ (reverseCustomControlFields hashMap (controlfields marc)) ++ (reverseCustomDataFields hashMap (datafields marc))
  | otherwise = (reverseCustomControlFields hashMap (controlfields marc)) ++ (reverseCustomDataFields hashMap (datafields marc))
  where leaderExists = fromJust (convert (fromJust (M.lookup "leader-exists" hashMap)))

reverseCustomLeader :: M.HashMap Name Value -> Leader -> String
reverseCustomLeader hashMap leader = leaderLabel ++ spaceAfterTagLeader ++ (leaderValue leader)
  where leaderLabel = fromJust (convert (fromJust (M.lookup "leader-label" hashMap)))
        spaceAfterTagLeader = fromJust (convert (fromJust (M.lookup "space-after-tag-leader" hashMap)))

reverseCustomControlFields :: M.HashMap Name Value -> [ControlField] -> String
reverseCustomControlFields hashMap [] = ""
reverseCustomControlFields hashMap (x:xs) = (reverseCustomControlField hashMap x) ++ "\n" ++ (reverseCustomControlFields hashMap xs)

reverseCustomControlField :: M.HashMap Name Value -> ControlField -> String
reverseCustomControlField hashMap controlfield = (cfNumberTag controlfield) ++ spaceAfterTagControl ++ (controlValue controlfield)
  where spaceAfterTagControl = fromJust (convert (fromJust (M.lookup "space-after-tag-control" hashMap)))

reverseCustomDataFields :: M.HashMap Name Value -> [DataField] -> String
reverseCustomDataFields hashMap [] = ""
reverseCustomDataFields hashMap (x:xs) = (reverseCustomDataField hashMap x) ++ "\n" ++ (reverseCustomDataFields hashMap xs)

reverseCustomDataField :: M.HashMap Name Value -> DataField -> String
reverseCustomDataField hashMap datafield
  | indicatorNeeded == True = (dfNumberTag datafield) ++ spaceAfterTagData ++ (ind1 datafield) ++ (ind2 datafield) ++ spaceAfterIndicator ++ (reverseCustomSubFields hashMap (subfields datafield))
  | indicatorNeeded == False = 
    case () of
     () | (indicator1 ++ indicator2) == "" -> (dfNumberTag datafield) ++ spaceAfterTagData ++ (reverseCustomSubFields hashMap (subfields datafield))
        | otherwise -> (dfNumberTag datafield) ++ spaceAfterTagData ++ indicator1 ++ indicator2 ++ spaceAfterIndicator ++ (reverseCustomSubFields hashMap (subfields datafield))
  where spaceAfterTagData = fromJust (convert (fromJust (M.lookup "space-after-tag-data" hashMap)))
        indicatorNeeded = fromJust (convert (fromJust (M.lookup "indicator-needed" hashMap)))
        spaceAfterIndicator = fromJust (convert (fromJust (M.lookup "space-after-indicator" hashMap)))
        indicator1 = ind1 datafield
        indicator2 = ind2 datafield

reverseCustomSubFields :: M.HashMap Name Value -> [SubField] -> String
reverseCustomSubFields hashMap [] = ""
reverseCustomSubFields hashMap (x:xs) = (reverseCustomSubField hashMap x) ++ (reverseCustomSubFields hashMap xs)

reverseCustomSubField :: M.HashMap Name Value -> SubField -> String
reverseCustomSubField hashMap subfield
  | subFieldEndExists == False = subFieldSymbol ++ (fieldTag subfield) ++ subFieldBeforeString ++ (fieldValue subfield) ++ subFieldAfterString
  | subFieldEndExists == True = subFieldSymbol ++ (fieldTag subfield) ++ subFieldEndSymbol ++ subFieldBeforeString ++ (fieldValue subfield) ++ subFieldAfterString
  where subFieldEndExists = fromJust (convert (fromJust (M.lookup "subfield-end-exists" hashMap)))
        subFieldSymbol = fromJust (convert (fromJust (M.lookup "subfield-symbol" hashMap)))
        subFieldEndSymbol = fromJust (convert (fromJust (M.lookup "subfield-end-symbol" hashMap)))
        subFieldBeforeString = fromJust (convert (fromJust (M.lookup "subfield-before-string" hashMap)))
        subFieldAfterString = fromJust (convert (fromJust (M.lookup "subfield-after-string" hashMap)))
                       
listToString :: [String] -> String
listToString [] = ""
listToString (x:xs)
    | null xs = x
    | head xs == "" = x
    | otherwise = x ++ " " ++ listToString xs


jsonFile :: FilePath
jsonFile = "output/marc21_out.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile
    

type Directions = [Int]

-- searchControl :: Doc -> String -> Int -> Directions
-- searchControl (Doc [] _) tag idx = []
-- searchControl (Doc (x:xs) format) tag idx
--   | searchResult == [] = searchControl (Doc xs format) tag (idx + 1)
--   | otherwise = idx:searchResult
--   where searchResult = searchControlMarc x tag 0

addControl :: Doc -> ControlField -> Int -> Doc
addControl (Doc myRecords format) cf marcIdx = (Doc (addControlList cf marcIdx myRecords) format)

addControlList :: ControlField -> Int -> [Marc] -> [Marc]
addControlList _ _ [] = []
addControlList cf marcIdx (x:xs)
  | marcIdx == 0 = (addControlMarc x cf):xs
  | otherwise = x:(addControlList cf (marcIdx - 1) xs)

addControlMarc :: Marc -> ControlField -> Marc
addControlMarc (Marc ldr cfs dfs format) cf = (Marc ldr newList dfs format)
  where addedList = cf:cfs
        sortedList = S.toSortedList addedList
        newList = S.fromSortedList sortedList

addData :: Doc -> DataField -> Int -> Doc
addData (Doc myRecords format) df marcIdx = (Doc (addDataList df marcIdx myRecords) format)

addDataList :: DataField -> Int -> [Marc] -> [Marc]
addDataList _ _ [] = []
addDataList df marcIdx (x:xs)
  | marcIdx == 0 = (addDataMarc x df):xs
  | otherwise = x:(addDataList df (marcIdx - 1) xs)

addDataMarc :: Marc -> DataField -> Marc
addDataMarc (Marc ldr cfs dfs format) df = (Marc ldr cfs newList format)
  where addedList = df:dfs
        sortedList = S.toSortedList addedList
        newList = S.fromSortedList sortedList

searchDataDataMarc :: Marc -> String -> Int -> Int
searchDataDataMarc (Marc _ _ [] _) tag idx = -1
searchDataDataMarc (Marc ldr cf (x:xs) format) tag idx
  | searchResult = idx
  | otherwise = searchDataDataMarc (Marc ldr cf xs format) tag (idx + 1)
  where searchDataData df tag1 = if (dfNumberTag df) == tag1 then True else False
        searchResult = searchDataData x tag

addSubfield :: Doc -> SubField -> String -> Int -> Int -> Doc
addSubfield doc@(Doc myRecords format) sf dfTag marcIdx dfOffset
  | directions == -1 = doc
  | otherwise = (Doc (addSubfieldList sf marcIdx (directions + dfOffset) myRecords) format)
  where directions = searchDataDataMarc (myRecords !! marcIdx) dfTag 0

addSubfieldList :: SubField -> Int -> Int -> [Marc] -> [Marc]
addSubfieldList _ _ _ [] = []
addSubfieldList sf marcIdx dfIdx (x:xs)
  | marcIdx == 0 = (addSubfieldMarc x sf dfIdx):xs
  | otherwise = x:(addSubfieldList sf (marcIdx - 1) dfIdx xs)
  where addSubfieldMarc (Marc ldr cfs dfs format) sf dfIdx = (Marc ldr cfs (addDataMarcList sf dfIdx dfs) format)

addDataMarcList :: SubField -> Int -> [DataField] -> [DataField]
addDataMarcList _ _ [] = []
addDataMarcList sf dfIdx (x:xs)
  | dfIdx == 0 = (addSubfieldDatafield x sf):xs
  | otherwise = x:(addDataMarcList sf (dfIdx - 1) xs)

addSubfieldDatafield :: DataField -> SubField -> DataField
addSubfieldDatafield (DataField tag ind1 ind2 sfs format) sf = (DataField tag ind1 ind2 newList format)
  where addedList = sf:sfs
        sortedList = S.toSortedList addedList
        newList = S.fromSortedList sortedList

changeLeader :: Doc -> String -> Int -> Doc
changeLeader doc@(Doc myRecords format) value marcIdx = (Doc (changeLeaderList value marcIdx myRecords) format)

changeLeaderList :: String -> Int -> [Marc] -> [Marc]
changeLeaderList _ _ [] = []
changeLeaderList value idx (x:xs)
  | idx == 0 = (changeLeaderMarc x value):xs
  | otherwise = x:(changeLeaderList value (idx - 1) xs)
  where changeLeaderMarc (Marc ldr cf df format) value = (Marc (changeLeaderValue ldr value) cf df format)
        changeLeaderValue (Leader ldrTag ldrValue ldrFormat) value1 = (Leader ldrTag value1 ldrFormat)

searchControlMarc :: Marc -> String -> Int -> Int
searchControlMarc (Marc _ [] _ _) tag idx = -1
searchControlMarc (Marc ldr (x:xs) df format) tag idx
  | searchResult = idx
  | otherwise = searchControlMarc (Marc ldr xs df format) tag (idx + 1)
  where searchControlControl cf tag1 = if (cfNumberTag cf) == tag1 then True else False
        searchResult = searchControlControl x tag

changeControl :: Doc -> String -> String -> Int -> Int -> Doc
changeControl doc@(Doc myRecords format) tag value marcIdx offset
  | directions == -1 = doc
  | otherwise = (Doc (changeControlList value marcIdx (directions + offset) myRecords) format)
  where directions = searchControlMarc ((records doc) !! marcIdx) tag 0

changeControlList :: String -> Int -> Int -> [Marc] -> [Marc]
changeControlList _ _ _ [] = []
changeControlList value myIdx otherIdx (x:xs)
  | myIdx == 0 = (changeControlMarc x value otherIdx):xs
  | otherwise = x:(changeControlList value (myIdx - 1) otherIdx xs)
  where changeControlMarc (Marc ldr cf df format) value idx1 = (Marc ldr (changeControlMarcList value idx1 cf) df format)

changeControlMarcList :: String -> Int -> [ControlField] -> [ControlField]
changeControlMarcList _ _ [] = []
changeControlMarcList value idx (x:xs)
  | idx == 0 = (replaceControl value x):xs
  | otherwise = x:(changeControlMarcList value (idx - 1) xs)
  where replaceControl value (ControlField tag _ format) = (ControlField tag value format)

searchIndMarc :: Marc -> String -> Int -> Int
searchIndMarc (Marc _ _ [] _) tag idx = -1
searchIndMarc (Marc ldr cf (x:xs) format) tag idx
  | searchResult = idx
  | otherwise = searchIndMarc (Marc ldr cf xs format) tag (idx + 1)
  where searchIndInd df = if (dfNumberTag df) == tag then True else False
        searchResult = searchIndInd x

changeInd :: Doc -> String -> String -> String -> Int -> Int -> Doc
changeInd doc@(Doc myRecords format) dfTag label value marcIdx dfOffset
  | directions == -1 = doc
  | otherwise = (Doc (changeIndList label value marcIdx (directions + dfOffset) myRecords) format)
  where directions = searchIndMarc ((records doc) !! marcIdx) dfTag 0

changeIndList :: String -> String -> Int -> Int -> [Marc] -> [Marc]
changeIndList _ _ _ _ [] = []
changeIndList label value myIdx dfIdx (x:xs)
  | myIdx == 0 = (changeIndMarc x label value dfIdx):xs
  | otherwise = x:(changeIndList label value (myIdx - 1) dfIdx xs)
  where changeIndMarc (Marc ldr cf df format) label value dfIdx = (Marc ldr cf (changeIndListInd label value dfIdx df) format)

changeIndListInd :: String -> String -> Int -> [DataField] -> [DataField]
changeIndListInd _ _ _ [] = []
changeIndListInd label value dfIdx (x:xs)
  | dfIdx == 0 = (changeIndInd x label value):xs
  | otherwise = x:(changeIndListInd label value (dfIdx - 1) xs)
  where changeIndInd (DataField tag ind1 ind2 sf format) label value = if label == "ind1" then (DataField tag value ind2 sf format) else (DataField tag ind1 value sf format)

searchDataMarc :: Marc -> String -> String -> Int -> Directions
searchDataMarc (Marc _ _ [] _) tag sfTag idx = []
searchDataMarc (Marc ldr cf (x:xs) format) tag sfTag idx
  | searchResult == [] = searchDataMarc (Marc ldr cf xs format) tag sfTag (idx + 1)
  | otherwise = idx:searchResult
  where searchDataData df tag1 sfTag1 = if (dfNumberTag df) == tag1 then (searchDataList df sfTag1 0) else []
        searchResult = searchDataData x tag sfTag

searchDataList :: DataField -> String -> Int -> Directions
searchDataList (DataField _ _ _ [] format) tag idx = []
searchDataList (DataField myTag ind1 ind2 (x:xs) format) tag idx
  | searchResult = [idx]
  | otherwise = searchDataList (DataField myTag ind1 ind2 xs format) tag (idx + 1)
  where searchDataSubfield sf tag1 = if (fieldTag sf) == tag1 then True else False
        searchResult = searchDataSubfield x tag

changeData :: Doc -> String -> String -> String -> Int -> Int -> Int -> Doc
changeData doc@(Doc myRecords format) dfTag sfTag value marcIdx dfOffset sfOffset
  | directions == [] = doc
  | otherwise = (Doc (changeDataList value marcIdx ((directions !! 0) + dfOffset) ((directions !! 1) + sfOffset) myRecords) format)
  where directions = searchDataMarc ((records doc) !! marcIdx) dfTag sfTag 0

changeDataList :: String -> Int -> Int -> Int -> [Marc] -> [Marc]
changeDataList _ _ _ _ [] = []
changeDataList value myIdx dfIdx sfIdx (x:xs)
  | myIdx == 0 = (changeDataMarc x value dfIdx sfIdx):xs
  | otherwise = x:(changeDataList value (myIdx - 1) dfIdx sfIdx xs)
  where changeDataMarc (Marc ldr cf df format) value idx1 idx2 = (Marc ldr cf (changeDataListData value dfIdx sfIdx df) format)

changeDataListData :: String -> Int -> Int -> [DataField] -> [DataField]
changeDataListData _ _ _ [] = []
changeDataListData value dfIdx sfIdx (x:xs)
  | dfIdx == 0 = (changeDataData x value sfIdx):xs
  | otherwise = x:(changeDataListData value (dfIdx - 1) sfIdx xs)
  where changeDataData (DataField tag ind1 ind2 sf format) value idx1 = (DataField tag ind1 ind2 (changeDataSubfieldList value idx1 sf) format)

changeDataSubfieldList :: String -> Int -> [SubField] -> [SubField]
changeDataSubfieldList _ _ [] = []
changeDataSubfieldList value idx (x:xs)
  | idx == 0 = (replaceData value x):xs
  | otherwise = x:(changeDataSubfieldList value (idx - 1) xs)
  where replaceData value (SubField tag _ format) = (SubField tag value format)

deleteControl :: Doc -> String -> Int -> Int -> Doc
deleteControl (Doc myRecords format) tag marcIdx offset = (Doc (deleteControlList tag marcIdx offset myRecords) format)

deleteControlList :: String -> Int -> Int -> [Marc] -> [Marc]
deleteControlList _ _ _ [] = []
deleteControlList tag marcIdx offset (x:xs)
  | marcIdx == 0 = (deleteControlMarc x tag offset):xs
  | otherwise = x:(deleteControlList tag (marcIdx - 1) offset xs)

deleteControlMarc :: Marc -> String -> Int -> Marc
deleteControlMarc marc@(Marc ldr cfs dfs format) tag offset
  | directions == -1 = marc
  | otherwise = (Marc ldr newList dfs format)
  where directions = searchControlMarc marc tag 0
        idx = directions + offset
        (ys, zs) = splitAt idx cfs
        newList = ys ++ (tail zs)

deleteData :: Doc -> String -> Int -> Int -> Doc
deleteData (Doc myRecords format) tag marcIdx offset = (Doc (deleteDataList tag marcIdx offset myRecords) format)

deleteDataList :: String -> Int -> Int -> [Marc] -> [Marc]
deleteDataList _ _ _ [] = []
deleteDataList tag marcIdx offset (x:xs)
  | marcIdx == 0 = (deleteDataMarc x tag offset):xs
  | otherwise = x:(deleteDataList tag (marcIdx - 1) offset xs)

deleteDataMarc :: Marc -> String -> Int -> Marc
deleteDataMarc marc@(Marc ldr cfs dfs format) tag offset
  | directions == -1 = marc
  | otherwise = (Marc ldr cfs newList format)
  where directions = searchDataDataMarc marc tag 0
        idx = directions + offset
        (ys, zs) = splitAt idx dfs
        newList = ys ++ (tail zs)

deleteSubfield :: Doc -> String -> String -> Int -> Int -> Int -> Doc
deleteSubfield (Doc myRecords format) dfTag sfTag marcIdx dfOffset sfOffset = (Doc (deleteSubfieldList dfTag sfTag marcIdx dfOffset sfOffset myRecords) format)

deleteSubfieldList :: String -> String -> Int -> Int -> Int -> [Marc] -> [Marc]
deleteSubfieldList _ _ _ _ _ [] = []
deleteSubfieldList dfTag sfTag marcIdx dfOffset sfOffset (x:xs)
  | marcIdx == 0 = (deleteSubfieldMarc x dfTag sfTag dfOffset sfOffset):xs
  | otherwise = x:(deleteSubfieldList dfTag sfTag (marcIdx - 1) dfOffset sfOffset xs)
          
deleteSubfieldMarc :: Marc -> String -> String -> Int -> Int -> Marc
deleteSubfieldMarc marc@(Marc ldr cfs dfs format) dfTag sfTag dfOffset sfOffset
  | directions == [] = marc
  | otherwise = (Marc ldr cfs (deleteSubfieldListData ((directions !! 0) + dfOffset) ((directions !! 1) + sfOffset) dfs) format)
  where directions = searchDataMarc marc dfTag sfTag 0

deleteSubfieldListData :: Int -> Int -> [DataField] -> [DataField]
deleteSubfieldListData _ _ [] = []
deleteSubfieldListData dfIdx sfIdx (x:xs)
  | dfIdx == 0 = (deleteSubfieldDatafield x sfIdx):xs
  | otherwise = x:(deleteSubfieldListData (dfIdx - 1) sfIdx xs)

deleteSubfieldDatafield :: DataField -> Int -> DataField
deleteSubfieldDatafield (DataField tag ind1 ind2 sfs format) sfIdx = (DataField tag ind1 ind2 newList format)
  where (ys, zs) = splitAt sfIdx sfs
        newList = ys ++ (tail zs)

-- main :: IO ()
-- main = do args <- getArgs
--           let input = head (tail args)
--               output = head (tail (tail args))
--               format = head (tail (tail (tail args)))
--           h <- openFile input ReadMode
--           hSetEncoding h utf8
--           cont <- hGetContents h
--           myConfig <- C.load ([C.Required "config.cfg"])
--           myMap <- C.getMap myConfig
--           case (runParser (document myMap format) 0 input cont) of
--             Left err -> putStrLn . show $ err
--             Right rss -> B.writeFile output . P.encodePretty $ rss

-- main :: IO ()
-- main = do d <- (A.eitherDecode <$> getJSON) :: IO (Either String Doc)
--           let a = fromRight (Doc [] (Just "")) d
--           myConfig <- C.load ([C.Required "config.cfg"])
--           myMap <- C.getMap myConfig
--           case d of
--             Left err -> putStrLn err
--             Right ps -> writeFile "bleja.txt" (reverseCustomDoc myMap a)

main :: IO ()
main = do args <- getArgs
          let tag = head (tail args)
          d <- (A.eitherDecode <$> getJSON) :: IO (Either String Doc)
          let a = fromRight (Doc [] (Just "")) d
          case d of
            Left err -> putStrLn err
            -- Right ps -> putStrLn . show $ searchDataMarc ((records a) !! 0) tag "a" 0
            -- Right ps -> writeFile "bleja.txt" . show $ changeData a tag "blejica" "a" 0 1 0
            -- Right ps -> writeFile "bleja.txt" . show $ changeLeader a tag 0
            -- Right ps -> writeFile "bleja.txt" . show $ changeInd a tag "ind1" "9" 0 0
            -- Right ps -> writeFile "bleja.txt" . show $ addData a (DataField "245" "#" "#" [] (Just "json")) 0
            -- Right ps -> writeFile "bleja.txt" . show $ addSubfield a (SubField "b" "DLC" (Just "json")) "040" 0 0
            -- Right ps -> writeFile "bleja.txt" . show $ deleteData a "245" 0 0
            Right ps -> writeFile "bleja.txt" . show $ deleteSubfield a "245" "a" 0 0 0
