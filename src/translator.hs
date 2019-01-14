import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.List.Split
import System.IO
import qualified Data.Text as T

hello = "Hello World!"

normalizeStrings file  = do
  content <- readFile file
  return (splitOn "\n" content)
  
verificandoTipo lista
  | head lista=="NOP" || head lista=="ADD" || head lista=="AND" || head lista=="OR" || head lista=="SUB" || head lista =="NEG" || head lista =="CPY" || head lista =="INPUT" || head lista=="OUTPUT"  = tipoR lista
  | head lista=="LRG" || head lista=="BLT" || head lista=="BGT" || head lista=="BEQ" || head lista=="BNE" || head lista=="JMP" = tipoI lista
  | head lista=="JMP" = tipoJ lista

tipoR lista
  | head lista=="NOP"= "0"
  | head lista=="ADD"= "0001" ++ showIntAtBase 2 intToDigit (toInt (head(tail lista))) "" ++ showIntAtBase 2 intToDigit (toInt (head(tail(tail lista)))) "" ++ showIntAtBase 2 intToDigit (toInt (last lista)) ""
  | head lista=="AND"= "0010" ++ showIntAtBase 2 intToDigit (toInt (head(tail lista))) "" ++ showIntAtBase 2 intToDigit (toInt (head(tail(tail lista)))) "" ++ showIntAtBase 2 intToDigit (toInt (last lista)) ""
  | head lista=="OR" = "0011" ++ showIntAtBase 2 intToDigit (toInt (head(tail lista))) "" ++ showIntAtBase 2 intToDigit (toInt (head(tail(tail lista)))) "" ++ showIntAtBase 2 intToDigit (toInt (last lista)) ""
  | head lista=="SUB"= "0100" ++ showIntAtBase 2 intToDigit (toInt (head(tail lista))) "" ++ showIntAtBase 2 intToDigit (toInt (head(tail(tail lista)))) "" ++ showIntAtBase 2 intToDigit (toInt (last lista)) ""
  | head lista=="NEG"= "0101" ++ showIntAtBase 2 intToDigit (toInt (head(tail lista))) "" ++ showIntAtBase 2 intToDigit (toInt (last lista)) ""
  | head lista=="NOT"= "0110" ++ showIntAtBase 2 intToDigit (toInt (head(tail lista))) "" ++ showIntAtBase 2 intToDigit (toInt (last lista)) ""
  | head lista=="CPY"= "0111" ++ showIntAtBase 2 intToDigit (toInt (head(tail lista))) "" ++ showIntAtBase 2 intToDigit (toInt (last lista)) ""
  | head lista=="INPUT" = "1110" 
  | head lista=="OUTPUT" = "1111"

tipoJ lista = "In progress"

tipoI lista = "In progress" 

toInt txt = read txt :: Int 

-- | @Param maxSize = Receive max size of string
-- | @Param binary = Receive binary string to be formated
-- | This function put leading zeros on a String of defined length
formatBinaryOutput maxSize binary
  | length binary > maxSize = error "Bit limit exceeded"
  | length binary == maxSize = binary
  | length binary < maxSize = formatBinaryOutput maxSize ("0" ++ binary)