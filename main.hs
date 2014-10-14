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


filterOnNumSyllables pronunciations = do
	putStrLn "Enter number of syllables:"
	syllables <- getLine
	let syllables' = read syllables :: Int
	mapM_ putStrLn $ [word | word <- pronunciations, numSyllables word == syllables'] 


filterOnStressSyllables pronunciations = do
	putStrLn "Enter syllable stresses (0 - no stress; 1 - primary stress; 2 - secondary stress):"
	pattern <- getLine
	--let regex = mkRegex $ "^[^\\d]+" ++ (intercalate "[^\\d]+" $ map return pattern) ++ "[^\\d]*$"
	--mapM_ putStrLn $ [word | word <- pronunciations, (isJust . matchRegex regex . phonemes) word]
	mapM_ putStrLn $ [word | word <- pronunciations, pattern == filter isDigit (phonemes word)]


main = do
	file <- readFile "cmudict.0.7a.txt"
	let pronunciations = filter (not . (==) ';' . head) $ lines file
	putStrLn "Search by number of syllables (1) or syllable pattern (2)?"
	choice <- getLine
	let fn = if (choice == "1") then filterOnNumSyllables else filterOnStressSyllables
	forever $ fn pronunciations