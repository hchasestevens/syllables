import Data.Char (isDigit)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Text.Regex (mkRegex, matchRegex)
import Control.Monad (forever)

phonemes :: String -> String
phonemes str = cs
	where
		(_ : cs : _) = splitOn "  " str


numSyllables :: String -> Int
numSyllables str = length $ filter isDigit $ phonemes str


comparePattern :: String -> String -> Bool
comparePattern [] (x:xs) = False
comparePattern (x:xs) [] = False
comparePattern [] [] = True
comparePattern (x:xs) (y:ys)
	| x == '*' = comparePattern xs ys
	| y == '*' = comparePattern xs ys
	| x == y = comparePattern xs ys
	| otherwise = False


filterOnNumSyllables pronunciations = do
	putStrLn "Enter number of syllables:"
	syllables <- getLine
	let syllables' = read syllables :: Int
	mapM_ putStrLn $ [word | word <- pronunciations, numSyllables word == syllables'] 
	putStrLn ""


filterOnStressSyllables pronunciations = do
	putStrLn "Enter syllable stresses (0 - none; 1 - primary; 2 - secondary; * - any):"
	pattern <- getLine
	--let regex = mkRegex $ "^[^\\d]+" ++ (intercalate "[^\\d]+" $ map return pattern) ++ "[^\\d]*$"
	--mapM_ putStrLn $ [word | word <- pronunciations, (isJust . matchRegex regex . phonemes) word]
	mapM_ putStrLn $ [word | word <- pronunciations, pattern `comparePattern` filter isDigit (phonemes word)]
	putStrLn ""


main = do
	file <- readFile "cmudict.0.7a.txt"
	let pronunciations = filter (not . (==) ';' . head) $ lines file
	putStrLn "Search by number of syllables (1) or syllable pattern (2)?"
	choice <- getLine
	let fn = if (choice == "1") then filterOnNumSyllables else filterOnStressSyllables
	forever $ fn pronunciations