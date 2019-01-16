import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Hex
import Data.List.Split
import System.IO
import qualified Data.Text as T

hello = "Hello World!"

normalizeStrings file  = do
  content <- readFile file
  return (splitOn "\n" content)
  
verificandoTipo lista
  | head lista=="NOP" || head lista=="ADD" || head lista=="AND" || head lista=="OR" || head lista=="SUB" || head lista =="NEG" || head lista =="CPY" || head lista =="INPUT" || head lista=="OUTPUT"  = tipoR lista
  | head lista=="LRG" || head lista=="BLT" || head lista=="BGT" || head lista=="BEQ" || head lista=="BNE" = tipoI lista
  | head lista=="JMP" = tipoJ lista

tipoR lista
  | head lista=="NOP"= "0"
  | head lista=="ADD"= binToHex (splitOn "" ("0001" ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail lista))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail(tail lista)))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (last lista)) "") ++ "000")) 0 0
  | head lista=="AND"= binToHex (splitOn "" ("0010" ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail lista))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail(tail lista)))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (last lista)) "") ++ "000")) 0 0
  | head lista=="OR" = binToHex (splitOn "" ("0011" ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail lista))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail(tail lista)))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (last lista)) "") ++ "000")) 0 0
  | head lista=="SUB"= binToHex (splitOn "" ("0100" ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail lista))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail(tail lista)))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (last lista)) "") ++ "000")) 0 0
  | head lista=="NEG"= binToHex (splitOn "" ("0101" ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail lista))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (last lista)) "") ++ "000000")) 0 0
  | head lista=="NOT"= binToHex (splitOn "" ("0110" ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail lista))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (last lista)) "") ++ "000000")) 0 0
  | head lista=="CPY"= binToHex (splitOn "" ("0111" ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail lista))) "") ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (last lista)) "") ++ "000000")) 0 0
  | head lista=="INPUT" = "1110" 
  | head lista=="OUTPUT" = binToHex (splitOn "" ("1111" ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail lista))) "") ++ "000000000")) 0 0

tipoJ lista = binToHex (splitOn "" ("11010000" ++ formatBinaryOutput 8 (showIntAtBase 2 intToDigit(toInt (last lista))"") )) 0 0

tipoI lista = "In progress" 

toInt txt = read txt :: Int 

-- | @Param maxSize = Receive max size of string
-- | @Param binary = Receive binary string to be formated
-- | This function put leading zeros on a String of defined length
formatBinaryOutput maxSize binary
  | length binary > maxSize = error "Bit limit exceeded"
  | length binary == maxSize = binary
  | length binary < maxSize = formatBinaryOutput maxSize ("0" ++ binary)

-- | @Param bin = Receive a list of Binary number, but the list must be separeted, like ["", "1", "0", "1", "1"], use splitOn "" <listToSplit>
-- | @Param pos = Receive position where the fuction is in the Binary list. Always must be started with 0
-- | @Param result = Receive the result, this must be started with 0.
-- | This function transform a String representing Binary into a String representing a Hexadecimal number.
binToHex bin pos result
  | last bin == "" = (showHex result "")
  | last bin == "0" = binToHex (init bin) (pos + 1) result
  | last bin == "1" = binToHex (init bin) (pos +1) (result + (1*(2^pos)))