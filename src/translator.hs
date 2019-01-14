import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.List.Split
import System.IO

hello = "Hello World!"

normalizeStrings file  = do
  content <- readFile file
  return (splitOn "\n" content)
  
verificandoTipo lista
  | head lista=="NOP" || head lista=="ADD" || head lista=="AND" || head lista=="OR" || head lista=="SUB" || head lista =="NEG" || head lista =="CPY" || head lista =="INPUT" || head lista=="OUTPUT"  = tipoR lista
  | head lista=="LRG" || head lista=="BLT" || head lista=="BGT" || head lista=="BEQ" || head lista=="BNE" || head lista=="JMP" = tipoI lista
  | head lista=="JMP" = tipoJ lista

tipoR lista
  | head lista=="NOP"= "Faz nada"
  | head lista=="ADD"= "0001"-- ++ toInt (head(tail lista)) ++ toInt (head(tail(tail lista)))
  | head lista=="AND"= "0010" 
  | head lista=="OR" = "0011"
  | head lista=="SUB"= "0100" ++ toInt (head(tail lista)) - toInt (head(tail(tail lista)))
  | head lista=="NEG"= "0101" ++ toInt (head(tail lista))*(-1)
  | head lista=="NOT"= "" -- ~R1
  | head lista=="CPY"= "NADA" -- -R1
  | head lista=="INPUT" = "DO IT"
  | head lista=="OUTPUT" = "DO IT"

tipoJ lista = "In progress"  


tipoI lista = "In progress" 

toInt txt = read txt :: Int 
