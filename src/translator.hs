import System.IO
import Data.List.Split

hello = "Hello World!"

handleFile file = readFile file

normalizeStrings text = splitOn "\n" text

