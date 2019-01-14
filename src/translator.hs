import System.IO
import Data.List.Split

hello = "Hello World!"

handleFile file = readFile file

normalizeStrings file = do 
  content <- handleFile file
  return (splitOn "\n" content)
