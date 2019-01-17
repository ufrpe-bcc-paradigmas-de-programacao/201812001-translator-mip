import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Hex
import Data.List.Split
import System.IO
import qualified Data.Text as T

hello = "Hello World!"

convert inputFile = do
  content <- readFile inputFile
  printArqFinal content
  
start comando = verificaTipo (splitOn " " comando)

retornaLista stringEntrada = loopDaLista (splitOn "\n" stringEntrada)  ""

loopDaLista list final 
  | (length list) <= 0 = final
  | (length list) > 0 = loopDaLista (tail list) ((("  " ++ show ((length list) - 1)) ++ "  :  " ++start (head list)) ++ ";\n" ++ final)

verificaTipo lista = case (head lista) of
  "NOP" -> tipoR lista  
  "ADD" -> tipoR lista  
  "AND" -> tipoR lista  
  "OR" -> tipoR lista 
  "SUB" -> tipoR lista  
  "NEG" -> tipoR lista  
  "CPY" -> tipoR lista
  "NOT" -> tipoR lista  
  "INPUT" -> tipoR lista  
  "OUTPUT" -> tipoR lista
  "LRG" -> tipoI lista  
  "BLT" -> tipoI lista  
  "BGT" -> tipoI lista  
  "BEQ" -> tipoI lista  
  "BNE" -> tipoI lista 
  "JMP" -> tipoJ lista 

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

tipoI lista
  | head lista=="LRG" = transf lista "1000"
  | head lista=="BLT" = transf lista "1001"
  | head lista=="BGT" = transf lista "1010"
  | head lista=="BEQ" = transf lista "1011"
  | head lista=="BNE" = transf lista "1100"
  
transf lista x =
  (if toInt(head(tail(tail lista))) < 0 
    then x ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail lista))) "") ++ "0" ++ "1" ++ formatBinaryOutput 8 (showIntAtBase 2 intToDigit (toInt (last (splitOn "-" (last lista)))) "")
    else x ++ formatBinaryOutput 3 (showIntAtBase 2 intToDigit (toInt (head(tail lista))) "") ++ "0" ++ formatBinaryOutput 8 (showIntAtBase 2 intToDigit (toInt (head(tail(tail lista)))) ""))

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

textoCompleto lista = ("-- Copyright (C) 1991-2013 Altera Corporation\n-- Your use of Altera Corporation's design tools, logic functions \n-- and other software and tools, and its AMPP partner logic \n-- functions, and any output files from any of the foregoing \n-- (including device programming or simulation files), and any \n-- associated documentation or information are expressly subject \n-- to the terms and conditions of the Altera Program License \n-- Subscription Agreement, Altera MegaCore Function License \n-- Agreement, or other applicable license agreement, including, \n-- without limitation, that your use is for the sole purpose of \n-- programming logic devices manufactured by Altera and sold by \n-- Altera or its authorized distributors.  Please refer to the \n-- applicable agreement for further details.\n-- Quartus II generated Memory Initialization File (.mif)\n\nWIDTH=16;\nDEPTH=256;\n\nADDRESS_RADIX=UNS;\nDATA_RADIX=HEX;\n\nCONTENT BEGIN\n" ++ lista ++ "  [" ++ (show ((getLengthSplited lista) - 1)) ++ "..255]  :   0000;\nEND;")

finalFunc fileContent = textoCompleto (retornaLista fileContent)

getLengthSplited text = length (splitOn "\n" text)

printArqFinal inputText = writeFile "./output/lrg.mif" (finalFunc inputText)
