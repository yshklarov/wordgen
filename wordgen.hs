module Main where

import qualified Data.Map as M
import System.Random
import Control.Monad
import Options.Applicative

-- Words are generated using a Markov chain, one character at a
-- time.
type Chain = M.Map State CharDistribution

-- The state is just the previous <statesize> characters, or fewer if
-- we are near the beginning of a word.
-- TODO: We could also keep other things in the state, like number of
-- characters so far, number of syllables, etc.
type State = String

-- A '\0' character output represents the end of a word.
-- The distribution is not normalized: it just holds the number of
-- times each character was encoutered.
type CharDistribution = M.Map Char Int

-- statesize must be at least 1. Larger values will generate more
-- realistic words, but if it's too large, the words will simply be
-- exact copies from the training file. Values of 3-4 seem to be
-- ideal.
data Opts = Opts
  { statesize :: Int
  , numwords :: Int 
  , trainingfile :: String }

optParser :: Parser Opts
optParser = Opts
  <$> option
      ( short 's'
     <> value 4
     <> metavar "<statesize>=4"
     <> help "The number of characters to keep in state when generating words" )
  <*> option
      ( short 'n'
     <> value 10
     <> metavar "<numwords>=10"
     <> help "The number of words to generate" )
  <*> argument Just
      ( metavar "training-file"
     <> help "File to train from; should contain one sample word per line" )


main = execParser opts >>= wordgen
  where
    opts = info (helper <*> optParser)
      ( fullDesc
     <> progDesc "Generate N random words similar to those in trainingfile"
     <> header "wordgen - a random word generator based on Markov chains" )

wordgen (Opts ssize numwords filename) = do
    chain <- trainChainFrom filename ssize
    let gw = genWord ssize chain
    -- TODO: fix uglyness (how?)
    let builder initgen =
          foldr (\_ (strs, oldgen) ->
                  (\(newstr, newgen) -> (newstr:strs, newgen))
                  (gw oldgen))
                ([], initgen)
                [1..numwords]
    words <- getStdRandom builder
    mapM_ putStrLn words


genWord :: RandomGen g => Int -> Chain -> g -> (String, g)
genWord ssize chain rgen = genWord' ssize "" chain rgen

genWord' :: RandomGen g => Int -> State -> Chain -> g -> (String, g)
genWord' ssize state chain rgen =
  let next = case (M.lookup state chain) of
         Nothing -> ('\NUL', rgen)
         Just cd -> genChar rgen cd
      in
    case next of
      ('\NUL', newrgen) -> ("", newrgen)
      (c, newrgen)      ->
        (c:rest, finalrgen) where
          (rest, finalrgen) = genWord' ssize (nextState ssize state c) chain newrgen

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


nextState :: Int -> State -> Char -> State
nextState ssize state nextChar
  | length state < ssize = state ++ [nextChar]
  | otherwise                = tail state ++ [nextChar]


-- Read a file, one word per line.
trainChainFrom :: String -> Int -> IO (Chain)
trainChainFrom filename ssize = do
  contents <- readFile filename
  return $ makeChain ssize $ lines contents
  

-- Train a Markov chain using a list of "example" words.
makeChain :: Int -> [String] -> Chain
makeChain ssize wordlist = foldr insertPoint M.empty
                           (concatMap (fractureWord ssize) wordlist)
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
