module Main where

import qualified Data.Map as M
import System.Random
import Control.Monad

-- Words are generated using a Markov chain, one character at a
-- time. The probabilities are stored in a lookup table.
type Chain = M.Map State CharDistribution

-- The state just contains the previous three characters, or fewer if
-- we are near the beginning of a word.
-- TODO: word length should be stored in the state, too.
type State = String

-- The distribution is not normalized.
type CharDistribution = M.Map Char Int

-- Eg. An empty string returns the distribution of first letters, and
-- a two-character string returns the distribution of the third letter
-- given the first two. If three characters are supplied, no
-- assumptions are made about where in the word we are.  A '\0'
-- character output represents the end of a word.
-- TODO: We could also keep other things in the state, like number of
-- characters so far, number of syllables, etc.


-- TODO: parse command line options instead of hard-coding this
engchain     = trainChainFrom "data/en/en-US.dic"
ruchain      = trainChainFrom "data/ru/ru.txt"
klingonchain = trainChainFrom "data/klingon/klingon.txt"
gcitieschain = trainChainFrom "data/cities/germancities.txt"
jcitieschain = trainChainFrom "data/cities/japancities.txt"

-- The number of characters to keep as state when generating words.
-- Must be at least 1. Larger values will generate more realistic
-- words, but if it's too large then the words will simply be exact
-- copies from the training database. Values of 3-4 seem to be ideal.
statesize :: Int
statesize = 4

main = do
  chain <- gcitieschain
  let words = map (fst . (flip genWord chain)) (map mkStdGen [0..100])
  mapM_ putStrLn words


genWord :: RandomGen g => g -> Chain -> (String, g)
genWord rgen chain = genWord' rgen "" chain

genWord' :: RandomGen g => g -> State -> Chain -> (String, g)
genWord' rgen state chain =
  let next = case (M.lookup state chain) of
         Nothing -> ('\NUL', rgen)
         Just cd -> genChar rgen cd
      in
    case next of
      ('\NUL', newrgen) -> ("", newrgen)
      (c, newrgen)      ->
        (c:rest, finalrgen) where
          (rest, finalrgen) = genWord' newrgen (nextState state c) chain

-- We assume a non-empty distribution is given.
genChar :: RandomGen g => g -> CharDistribution -> (Char, g)
genChar rgen cd = (getNth n alist, newgen)
  where
    (n, newgen) = randomR (1, total) rgen
    alist = M.assocs cd
    total = foldr ((+) . snd) 0 alist
    getNth :: Int -> [(Char, Int)] -> Char
    getNth n pool = let (ch, count) = head pool in
      if n <= count then ch else getNth (n-count) (tail pool)


nextState :: State -> Char -> State
nextState state nextChar
  | length state < statesize = state ++ [nextChar]
  | otherwise                = tail state ++ [nextChar]


-- Read a file, one word per line.
trainChainFrom :: String -> IO (Chain)
trainChainFrom filename = do
  contents <- readFile filename
  return $ makeChain $ lines contents
  

-- Train a Markov chain using a list of "example" words.
makeChain :: [String] -> Chain
makeChain wordlist = foldr insertPoint M.empty
                           (concatMap (fractureWord statesize) wordlist)
  where insertPoint (state, sampleChar) chain = M.insert state
          (case M.lookup state chain of
              Nothing -> M.fromList [(sampleChar, 1)]
              Just cdist -> addToCharDistribution sampleChar 1 cdist)
          chain
        addToCharDistribution :: Char -> Int -> CharDistribution -> CharDistribution
        addToCharDistribution ch i cdist =
          case M.lookup ch cdist of
              Nothing -> M.insert ch i cdist
              Just j -> M.insert ch (i+j) cdist

-- Break down a word into n-character sequences and return the
-- sequences together with the letters that follow each. Example:
-- fractureWord 3 "Dodo" = [("",'o'), ("D", 'o'), ("Do", 'd'),
--                          ("Dod", 'o'), ("odo", '\0')]
fractureWord :: Int -> String -> [(State, Char)]
fractureWord n str
  | length str <= n  = (map (splitter . flip take str) [1..length str]) ++
                       [(str, '\0')]
  | otherwise        = map (splitter . flip take str) [1..n] ++ fractureWord' n str
  where
    splitter s      = (init s, last s)

fractureWord' :: Int -> String -> [(String, Char)]
fractureWord' n str
  | length str == n  = (str, '\0') : []
  | otherwise        = (take n str, str !! n) : fractureWord' n (tail str)
