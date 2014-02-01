module Main where

import System.Random
import qualified Data.Map as M

-- Words are generated using a Markov chain, one character at a
-- time. The probabilities are stored in a lookup table.
type Chain = M.Map State CharDistribution

-- The state just contains the previous three characters, or fewer if
-- we are near the beginning of a word.
type State = String

-- The distribution is not normalized.
type CharDistribution = [(Char, Int)]

-- Eg. An empty string returns the distribution of first letters, and
-- a two-character string returns the distribution of the third letter
-- given the first two. If three characters are supplied, no
-- assumptions are made about where in the word we are.  A '\0'
-- character output represents the end of a word.
-- TODO: We could also keep other things in the state, like number of
-- characters so far, number of syllables, etc.

main = putStrLn $ show 3

-- Train a Markov chain using a list of "example" words.
makeChain :: [String] -> Chain
makeChain wordlist = foldr scanword M.empty wordlist
  where scanword word map =
          M.insert [head word] [(head word, 1)] map

-- Break down a word into 3-character sequences and return the
-- sequences together with the letters that follow each. Example:
-- fractureWord "Dodo" = [("",'o'), ("D", 'o'), ("Do", 'd'),
--                        ("Dod", 'o'), ("odo", '\0')]
fractureWord :: String -> [(String, Char)]
fractureWord [] = []
fractureWord (a:[]) = [([a], '\0')]
fractureWord (a:b:[]) = [([a], b), ([a, b], '\0')]
fractureWord (a:b:c:[]) = [([a], b), ([a,b], c), ([a,b,c], '\0')]
--fractureWord (a:b:c:xs) = [([a], b), ([a,b], c), ([a,b,c], '\0')]