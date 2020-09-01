{-# LANGUAGE OverloadedStrings #-}

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


-- Grammar
data Doc = Doc [Marc] Format

data Marc = Marc Leader [ControlField] [DataField] Format

data Leader = Leader LeaderTag LeaderValue Format

data ControlField = ControlField NumberTag ControlValue Format

data DataField = DataField NumberTag Indicator Indicator [SubField] Format

data SubField = SubField FieldTag FieldValue Format


-- Type synonyms
type LeaderTag = String
type LeaderValue = String
type NumberTag = String
type ControlValue = String
type Indicator = Char
type FieldTag = Char
type FieldValue = String
type NumTabs = Int
type Format = String


-- Deriving the Show typeclass for writing in JSON format
instance Show Doc where
    show (Doc records format)
      | format == "json" = "[\n" ++ showRecordsJSON records ++ "]"
      | format == "xml" = "<collection>\n" ++ showRecordsXML records ++ "</collection>"
      | format == "yaml" = showRecordsYAML records
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
      | format == "json" = "\t{\n" ++ show ldr ++ ",\n\t\t\"fields\":\n\t\t[" ++ showCntrlJSON cntrl ++ showDataJSON dataFlds ++ "\n\t\t]\n\t}"
      | format == "xml" = "  <record>\n" ++ show ldr ++ "\n" ++ showCntrlXML cntrl ++ showDataXML dataFlds ++ "  </record>"
      | format == "yaml" = "-\n" ++ show ldr ++ "\n" ++ "  controlfields:\n" ++ showCntrlYAML cntrl ++ "  datafields:\n" ++ showDataYAML dataFlds
        where showCntrlJSON [] = ""
              showCntrlJSON (x:xs)
                | null xs = if null dataFlds then "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t}" else "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t},"
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
      | format == "json" = "\t\t\"leader\":" ++ show value
      | format == "xml" = "    <leader>" ++ id value ++ "</leader>"
      | format == "yaml" = "  leader: " ++ id value
    
instance Show ControlField where
    show (ControlField tag value format)
      | format == "json" = "\t\t\t\t" ++ show tag ++ ":" ++  show value
      | format == "xml" = "    <controlfield tag=" ++ show tag ++ ">" ++ id value ++ "</controlfield>"
      | format == "yaml" = "      " ++ id tag ++ ": " ++ id value
   
instance Show DataField where
    show (DataField tag ind1 ind2 subFlds format)
      | format == "json" = "\t\t\t\t" ++ show tag ++ ":\n\t\t\t\t{\n\t\t\t\t\t\"ind1\":" ++ show (charToString ind1) ++ ",\n\t\t\t\t\t\"ind2\":" ++ show (charToString ind2) ++ ",\n\t\t\t\t\t\"subfields\":\n\t\t\t\t\t[" ++ showSubFldsJSON subFlds ++ "\n\t\t\t\t\t]\n\t\t\t\t}"
      | format == "xml" = "    <datafield tag=" ++ show tag ++ " ind1=" ++ show (charToString ind1) ++ " ind2=" ++ show (charToString ind2) ++ ">\n" ++ showSubFldsXML subFlds ++ "    </datafield>"
      | format == "yaml" = "    -\n" ++ "      " ++ id tag ++ ":\n" ++ "        -\n          ind1: " ++ id (charToString ind1) ++ "\n        -\n          ind2: " ++ id (charToString ind2) ++ "\n        -\n          subfields:\n" ++ showSubFldsYAML subFlds
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
      | format == "json" = "\t\t\t\t\t\t\t" ++ show (charToString tag) ++ ":" ++ show value
      | format == "xml" = "      <subfield code=" ++ show (charToString tag) ++ ">" ++ id value ++ "</subfield>"
      | format == "yaml" = "            " ++ id (charToString tag) ++ ": " ++ id value

    
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
                             return (Doc res format)

              
-- Marc21              
marc21 :: Format -> Parsec String Int Marc
marc21 format = do spaces
                   ldr <- leader21 format
                   spaces
                   cntrl <- many (try (cntFld21 format))
                   spaces
                   dataFlds <- many (try (dataFld21 format))
                   spaces
                   return (Marc ldr cntrl dataFlds format)
            
leader21 :: Format -> Parsec String Int Leader
leader21 format = do tag <- string "LDR"
                     spaces
                     value <- many (noneOf "\n")
                     return (Leader tag value format)
              
cntFld21 :: Format -> Parsec String Int ControlField
cntFld21 format = do tag <- count 3 digit
                     space
                     space
                     value <- many (noneOf "\n")
                     spaces
                     return (ControlField tag value format)
              
dataFld21 :: Format -> Parsec String Int DataField
dataFld21 format = do tag <- count 3 digit
                      spaces
                      ind1 <- char '#' <|> digit
                      ind2 <- char '#' <|> digit
                      spaces
                      subFlds <- many1 (subFld21 format)
                      return (DataField tag ind1 ind2 subFlds format)

subFld21 :: Format -> Parsec String Int SubField
subFld21 format = do char '$'
                     tag <- alphaNum
                     spaces
                     value <- many (noneOf "$\n")
                     spaces
                     return (SubField tag value format)
             
             
-- Marc21 - Library of Congress            
marc21congress :: Format -> Parsec String Int Marc
marc21congress format = do spaces
                           ldr <- leader21congress format
                           spaces
                           cntrl <- many (try (cntFld21congress format))
                           spaces
                           dataFlds <- many (try (dataFld21congress format))
                           spaces
                           return (Marc ldr cntrl dataFlds format)

leader21congress :: Format -> Parsec String Int Leader
leader21congress format = do tag <- string "000"
                             spaces
                             value <- many (noneOf "\n")
                             return (Leader tag value format)  

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
                             return (ControlField tag value format)

dataFld21congress :: Format -> Parsec String Int DataField
dataFld21congress format = do tag <- count 3 digit
                              spaces
                              ind1 <- char '_' <|> digit
                              ind2 <- char '_' <|> digit
                              spaces
                              subFlds <- many1 (subFld21congress format)
                              return (DataField tag ind1 ind2 subFlds format)

subFld21congress :: Format -> Parsec String Int SubField
subFld21congress format = do char '|'
                             tag <- alphaNum
                             spaces
                             value <- sepBy (many (noneOf " |\n")) (string " ")
                             spaces
                             return (SubField tag (listToString value) format)
            
            
-- Marc21 with LEADER                    
marc21leader :: Format -> Parsec String Int Marc
marc21leader format = do spaces
                         ldr <- leader21leader format
                         spaces
                         cntrl <- many (try (cntFld21leader format))
                         spaces
                         dataFlds <- many (try (dataFld21leader format))
                         spaces
                         return (Marc ldr cntrl dataFlds format)

leader21leader :: Format -> Parsec String Int Leader
leader21leader format = do tag <- string "LEADER"
                           spaces
                           value <- many (noneOf "\n")
                           return (Leader tag value format)

cntFld21leader :: Format -> Parsec String Int ControlField
cntFld21leader format = do tag <- count 3 digit
                           spaces
                           lookAhead (do count 2 (noneOf "$")
                                         spaces
                                         noneOf "$")
                           value <- many (noneOf "\n")
                           spaces
                           return (ControlField tag value format)                    

dataFld21leader :: Format -> Parsec String Int DataField
dataFld21leader format = do tag <- count 3 digit
                            spaces
                            ind1 <- option ('#') (try digit)
                            spaces
                            ind2 <- option ('#') (try digit)
                            spaces
                            subFlds <- many1 (subFld21leader format)
                            return (DataField tag ind1 ind2 subFlds format)

subFld21leader :: Format -> Parsec String Int SubField
subFld21leader format = do char '$'
                           tag <- alphaNum
                           spaces
                           value <- sepBy (many (noneOf " $\n")) (string " ")
                           spaces
                           return (SubField tag (listToString value) format)
      
      
-- UNIMARC                    
unimarc :: Format -> Parsec String Int Marc
unimarc format = do spaces
                    let ldr = (Leader "" "" format)
                    spaces
                    unildr <- unimarcleader format
                    spaces
                    dataFlds <- many1 (try (dataFldunimarc format))
                    spaces
                    return (Marc ldr [] (unildr:dataFlds) format)
 
unimarcleader :: Format -> Parsec String Int DataField
unimarcleader format = do tag <- string "001"
                          spaces
                          ind1 <- char '#' <|> digit
                          ind2 <- char '#' <|> digit
                          spaces
                          subFlds <- many (subFldunimarc format)
                          return (DataField tag ind1 ind2 subFlds format) 

dataFldunimarc :: Format -> Parsec String Int DataField
dataFldunimarc format = do lookAhead (do spaces
                                         notFollowedBy (string "001"))
                           tag <- count 3 digit
                           spaces
                           ind1 <- char '#' <|> digit
                           ind2 <- char '#' <|> digit
                           spaces
                           subFlds <- many1 (subFldunimarc format)
                           return (DataField tag ind1 ind2 subFlds format)

subFldunimarc :: Format -> Parsec String Int SubField
subFldunimarc format = do char '['
                          tag <- alphaNum
                          char ']'
                          value <- many (noneOf "[\n")
                          spaces
                          return (SubField tag value format)
        
        
-- UNIMARC inline             
unimarcsame :: Format -> Parsec String Int Marc
unimarcsame format = do spaces
                        let ldr = (Leader "" "" format)
                        spaces
                        dataFlds <- many1 (try (dataFldunimarcsame format))
                        return (Marc ldr [] dataFlds format)                                                                                                      
                    
dataFldunimarcsame :: Format -> Parsec String Int DataField
dataFldunimarcsame format = do tag <- count 3 digit
                               ind1 <- digit <|> space
                               ind2 <- digit <|> space
                               subFlds <- many (subFldunimarcsame format)
                               many (char (toEnum 30))
                               return (DataField tag ind1 ind2 subFlds format)                                        
                   
subFldunimarcsame :: Format -> Parsec String Int SubField
subFldunimarcsame format = do char (toEnum 31)
                              tag <- alphaNum
                              value <- many (noneOf ([toEnum 31]++[toEnum 30]++"\n"))
                              return (SubField tag value format)


-- Aleph Sequential
alephSeq :: Format -> Parsec String Int Marc
alephSeq format = do spaces
                     ldr <- ldrAlephSeq format
                     spaces
                     cntrl <- many (try (cntFldAlephSeq format))
                     spaces
                     dataFlds <- many (try (dataFldAlephSeq format))
                     spaces
                     return (Marc ldr cntrl dataFlds format)

ldrAlephSeq :: Format -> Parsec String Int Leader
ldrAlephSeq format = do count 9 digit
                        space
                        tag <- string "LDR"
                        spaces
                        char 'L'
                        spaces
                        value <- many (noneOf "\n")
                        return (Leader tag value format)

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
                           return (ControlField tag value format)

dataFldAlephSeq :: Format -> Parsec String Int DataField
dataFldAlephSeq format = do count 9 digit
                            space
                            tag <- count 3 digit
                            ind1 <- option ('#') (try digit)
                            spaces
                            ind2 <- option ('#') (try digit)
                            spaces
                            char 'L'
                            spaces
                            subFlds <- many1 (subFldAlephSeq format)
                            return (DataField tag ind1 ind2 subFlds format)

subFldAlephSeq :: Format -> Parsec String Int SubField
subFldAlephSeq format = do string "$$"
                           spaces
                           tag <- alphaNum
                           spaces
                           value <- many (noneOf "$\n")
                           spaces
                           return (SubField tag value format)


-- Custom format
customFormat :: M.HashMap Name Value -> Format -> Parsec String Int Marc
customFormat hashMap format = do spaces
                                 let a = fromJust (convert (fromJust (M.lookup "leader-exists" hashMap)))
                                 let b = if a then (customLeader hashMap format) else (return (Leader "" "" format))
                                 ldr <- b
                                 spaces
                                 cntrl <- many (try (customControlField hashMap format))
                                 spaces
                                 dataFlds <- many1 (try (customDataField hashMap format))
                                 return (Marc ldr cntrl dataFlds format)

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
                                 return (Leader tag value format)

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
                                       return (ControlField tag value format)

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
                                    let ind1Help = if a then (char (stringToChar indSymbol) <|> digit) else (option ('#') (try digit))
                                    let ind2Help = if a then (char (stringToChar indSymbol) <|> digit) else (option ('#') (try digit))
                                    ind1 <- ind1Help
                                    ind2 <- ind2Help
                                    spaces
                                    count dataFldValuePrefixLen anyChar
                                    spaces
                                    subFlds <- many1 (customSubfield hashMap format)
                                    return (DataField tag ind1 ind2 subFlds format)

customSubfield :: M.HashMap Name Value -> Format -> Parsec String Int SubField
customSubfield hashMap format = do let subFldSymbol = fromJust (convert (fromJust (M.lookup "subfield-symbol" hashMap)))
                                   let subFldSymbolEndExists = fromJust (convert (fromJust (M.lookup "subfield-end-exists" hashMap)))
                                   let subFldSymbolEnd = fromJust (convert (fromJust (M.lookup "subfield-end-symbol" hashMap)))
                                   string subFldSymbol
                                   tag <- alphaNum
                                   if subFldSymbolEndExists then (do string subFldSymbolEnd
                                                                     spaces) else spaces
                                   spaces
                                   value <- many (noneOf (subFldSymbol ++ "\n"))
                                   spaces
                                   return (SubField tag value format)

                       
listToString :: [String] -> String
listToString [] = ""
listToString (x:xs)
    | null xs = x
    | head xs == "" = x
    | otherwise = x ++ " " ++ listToString xs
    
          
main :: IO ()
main = do args <- getArgs
          let input = head (tail args)
              output = head (tail (tail args))
              format = head (tail (tail (tail args)))
          h <- openFile input ReadMode
          hSetEncoding h utf8
          cont <- hGetContents h
          myConfig <- C.load ([C.Required "config.cfg"])
          myMap <- C.getMap myConfig
          case (runParser (document myMap format) 0 input cont) of
            Left err -> putStrLn . show $ err
            Right rss -> writeFile output . show $ rss