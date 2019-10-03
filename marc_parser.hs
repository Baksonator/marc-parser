import Text.Parsec
import System.Environment
import Data.Char
import System.IO


-- Grammar
data Doc = Doc [Marc]

data Marc = Marc Leader [ControlField] [DataField]

data Leader = Leader LeaderTag LeaderValue

data ControlField = ControlField NumberTag ControlValue

data DataField = DataField NumberTag Indicator Indicator [SubField]

data SubField = SubField FieldTag FieldValue


-- Type synonyms
type LeaderTag = String
type LeaderValue = String
type NumberTag = String
type ControlValue = String
type Indicator = Char
type FieldTag = Char
type FieldValue = String
type NumTabs = Int


-- Deriving the Show typeclass for writing in JSON format
instance Show Doc where
    show (Doc records) = "[\n" ++ showRecords records ++ "]"
        where showRecords [] = ""
              showRecords (x:xs)
                | null xs = show x ++ "\n"
                | otherwise = show x ++ ",\n" ++ showRecords xs

instance Show Marc where
    show (Marc ldr cntrl dataFlds) = "\t{\n" ++ show ldr ++ ",\n\t\t\"fields\":\n\t\t[" ++ showCntrl cntrl ++ showData dataFlds ++ "\n\t\t]\n\t}"
        where showCntrl [] = ""
              showCntrl (x:xs)
                | null xs = if null dataFlds then "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t}" else "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t},"
                | otherwise = "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t}," ++ showCntrl xs
              showData [] = ""
              showData (x:xs)
                | null xs = "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t}"
                | otherwise = "\n\t\t\t{\n" ++ show x ++ "\n\t\t\t}," ++ showData xs
                          
instance Show Leader where
    show (Leader tag value) = "\t\t\"leader\":" ++ show value
    
instance Show ControlField where
    show (ControlField tag value) = "\t\t\t\t" ++ show tag ++ ":" ++  show value
   
instance Show DataField where
    show (DataField tag ind1 ind2 subFlds) = "\t\t\t\t" ++ show tag ++ ":\n\t\t\t\t{\n\t\t\t\t\t\"ind1\":" ++ show (charToString ind1) ++ ",\n\t\t\t\t\t\"ind2\":" ++ show (charToString ind2) ++ ",\n\t\t\t\t\t\"subfields\":\n\t\t\t\t\t[" ++ showSubFlds subFlds ++ "\n\t\t\t\t\t]\n\t\t\t\t}"
        where showSubFlds [] = ""
              showSubFlds (x:xs)
                | null xs = "\n\t\t\t\t\t\t{\n" ++ show x ++ "\n\t\t\t\t\t\t}"
                | otherwise = "\n\t\t\t\t\t\t{\n" ++ show x ++ "\n\t\t\t\t\t\t}," ++ showSubFlds xs
                          
instance Show SubField where
    show (SubField tag value) = "\t\t\t\t\t\t\t" ++ show (charToString tag) ++ ":" ++ show value

    
charToString :: Char -> String
charToString c = [c]
    
    
-- Parser
document :: Parsec String Int Doc
document = do spaces
              res <- many (try marc21 <|> try marc21congress <|> try marc21leader <|> try unimarc <|> try unimarcsame)
              spaces
              eof
              return (Doc res)

              
-- Marc21              
marc21 :: Parsec String Int Marc
marc21 = do spaces
            ldr <- leader21
            spaces
            cntrl <- many (try cntFld21)
            spaces
            dataFlds <- many (try dataFld21)
            spaces
            return (Marc ldr cntrl dataFlds)
            
leader21 :: Parsec String Int Leader
leader21 = do tag <- string "LDR"
              spaces
              value <- many (noneOf "\n")
              return (Leader tag value)
              
cntFld21 :: Parsec String Int ControlField
cntFld21 = do tag <- count 3 digit
              space
              space
              value <- many (noneOf "\n")
              spaces
              return (ControlField tag value)
              
dataFld21 :: Parsec String Int DataField
dataFld21 = do tag <- count 3 digit
               spaces
               ind1 <- char '#' <|> digit
               ind2 <- char '#' <|> digit
               spaces
               subFlds <- many1 subFld21
               return (DataField tag ind1 ind2 subFlds)

subFld21 :: Parsec String Int SubField
subFld21 = do char '$'
              tag <- alphaNum
              spaces
              value <- many (noneOf "$\n")
              spaces
              return (SubField tag value)
             
             
-- Marc21 - Library of Congress            
marc21congress :: Parsec String Int Marc
marc21congress = do spaces
                    ldr <- leader21congress
                    spaces
                    cntrl <- many (try cntFld21congress)
                    spaces
                    dataFlds <- many (try dataFld21congress)
                    spaces
                    return (Marc ldr cntrl dataFlds)

leader21congress :: Parsec String Int Leader
leader21congress = do tag <- string "000"
                      spaces
                      value <- many (noneOf "\n")
                      return (Leader tag value)                    

cntFld21congress :: Parsec String Int ControlField
cntFld21congress = do tag <- count 3 digit
                      spaces
                      lookAhead (do count 2 anyChar
                                    spaces
                                    notFollowedBy (char '|'))
                      lookAhead (do count 2 anyChar
                                    spaces
                                    notFollowedBy (char '['))
                      value <- many (noneOf "\n")
                      spaces
                      return (ControlField tag value)

dataFld21congress :: Parsec String Int DataField
dataFld21congress = do tag <- count 3 digit
                       spaces
                       ind1 <- char '_' <|> digit
                       ind2 <- char '_' <|> digit
                       spaces
                       subFlds <- many1 subFld21congress
                       return (DataField tag ind1 ind2 subFlds)

subFld21congress :: Parsec String Int SubField
subFld21congress = do char '|'
                      tag <- alphaNum
                      spaces
                      value <- sepBy (many (noneOf " |\n")) (string " ")
                      spaces
                      return (SubField tag (listToString value))
            
            
-- Marc21 with LEADER                    
marc21leader :: Parsec String Int Marc
marc21leader = do spaces
                  ldr <- leader21leader
                  spaces
                  cntrl <- many (try cntFld21leader)
                  spaces
                  dataFlds <- many (try dataFld21leader)
                  spaces
                  return (Marc ldr cntrl dataFlds)

leader21leader :: Parsec String Int Leader
leader21leader = do tag <- string "LEADER"
                    spaces
                    value <- many (noneOf "\n")
                    return (Leader tag value)

cntFld21leader :: Parsec String Int ControlField
cntFld21leader = do tag <- count 3 digit
                    spaces
                    lookAhead (do count 2 (noneOf "$")
                                  spaces
                                  noneOf "$")
                    value <- many (noneOf "\n")
                    spaces
                    return (ControlField tag value)                    

dataFld21leader :: Parsec String Int DataField
dataFld21leader = do tag <- count 3 digit
                     spaces
                     ind1 <- option ('#') (try digit)
                     spaces
                     ind2 <- option ('#') (try digit)
                     spaces
                     subFlds <- many1 subFld21leader
                     return (DataField tag ind1 ind2 subFlds)

subFld21leader :: Parsec String Int SubField
subFld21leader = do char '$'
                    tag <- alphaNum
                    spaces
                    value <- sepBy (many (noneOf " $\n")) (string " ")
                    spaces
                    return (SubField tag (listToString value))
      
      
-- UNIMARC                    
unimarc :: Parsec String Int Marc
unimarc = do spaces
             let ldr = (Leader "" "")
             spaces
             unildr <- unimarcleader
             spaces
             dataFlds <- many1 (try dataFldunimarc)
             spaces
             return (Marc ldr [] (unildr:dataFlds))
 
unimarcleader :: Parsec String Int DataField
unimarcleader = do tag <- string "001"
                   spaces
                   ind1 <- char '#' <|> digit
                   ind2 <- char '#' <|> digit
                   spaces
                   subFlds <- many subFldunimarc
                   return (DataField tag ind1 ind2 subFlds) 

dataFldunimarc :: Parsec String Int DataField
dataFldunimarc = do lookAhead (do spaces
                                  notFollowedBy (string "001"))
                    tag <- count 3 digit
                    spaces
                    ind1 <- char '#' <|> digit
                    ind2 <- char '#' <|> digit
                    spaces
                    subFlds <- many1 subFldunimarc
                    return (DataField tag ind1 ind2 subFlds)

subFldunimarc :: Parsec String Int SubField
subFldunimarc = do char '['
                   tag <- alphaNum
                   char ']'
                   value <- many (noneOf "[\n")
                   spaces
                   return (SubField tag value)
        
        
-- UNIMARC inline             
unimarcsame :: Parsec String Int Marc
unimarcsame = do spaces
                 let ldr = (Leader "" "")
                 spaces
                 dataFlds <- many1 (try dataFldunimarcsame)
                 return (Marc ldr [] dataFlds)                                                                                                      
                    
dataFldunimarcsame :: Parsec String Int DataField
dataFldunimarcsame = do tag <- count 3 digit
                        ind1 <- digit <|> space
                        ind2 <- digit <|> space
                        subFlds <- many subFldunimarcsame
                        many (char (toEnum 30))
                        return (DataField tag ind1 ind2 subFlds)                                        
                   
subFldunimarcsame :: Parsec String Int SubField
subFldunimarcsame = do char (toEnum 31)
                       tag <- alphaNum
                       value <- many (noneOf ([toEnum 31]++[toEnum 30]++"\n"))
                       return (SubField tag value)

                       
listToString :: [String] -> String
listToString [] = ""
listToString (x:xs)
    | null xs = x
    | head xs == "" = x
    | otherwise = x ++ " " ++ listToString xs
    
            
main :: IO ()
main = do (input:output:[]) <- getArgs
          h <- openFile input ReadMode
          hSetEncoding h utf8
          cont <- hGetContents h
          case (runParser document 0 input cont) of
            Left err -> putStrLn . show $ err
            Right rss -> writeFile output . show $ rss
            