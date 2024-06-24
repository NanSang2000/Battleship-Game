{-
----------------------------------------------------------------------
Purpose:  Playa simple two-player logical guessing game with Battleship Game on a 4×8 grid. 

%-----------------------------------------------------------------------------   
Project Description:

The Battleship Game involves two players: a searcher and a hider. 
The objective for the searcher is to locate three battleships based on feedback from the hider. 
This feedback consists of three numbers indicating how many of the searcher's three guesses either directly hit a battleship, 
were one unit away, or were two units away from the battleships' hidden locations. 
The game repeats these steps until the searcher successfully finds all three battleships.

The Game must meet the following criteria:
1. The three ships will be at three different locations.
2. Your guess must consist of exactly three different locations.
3. Your list of locations may be written in any order, but the order is not significant;

There is an example ship locations, guesses, and the feedback provided by the hider:

| Locations |	  Guess    |	Feedback
|H1, B2, D3 |	B3, C3, H3 |	0, 2, 1
|H1, B2, D3 |	B1, A2, H3 |	0, 2, 1
|H1, B2, D3 |	B2, H2, H1 |	2, 1, 0
|A1, D2, B3 | A3, D2, H1 |	1, 1, 0
|A1, D2, B3 |	H4, G3, H2 |	0, 0, 0
|A1, D2, B3 |	D2, B3, A1 |	3, 0, 0

 -}
module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

-- library The Data library be added in this programme to use the functions. 
import Data.List (sort, sortBy, group, delete)
import Data.Function (on)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Definition of a Location on a grid, by a column ('A'-'H') and a row (1-4).
data Location = Location Char Int
    deriving (Ord, Eq, Show)

-- GameState defined possible game states, each being a list of Locations
type GameState = [[Location]]

-- toLocation function converts a string to a Location, if it conforms to expected grid coordinates.
toLocation :: String -> Maybe Location
toLocation str
    | length str == 2,
      let [a,b] = str,
      a `elem` ['A'..'H'],
      b `elem` ['1'..'4']= Just (Location a (read [b])) -- Properly formatted input creates a Location.
    | otherwise = Nothing -- Invalid input returns Nothing.

-- fromLocation function converts a Location to a string.
fromLocation :: Location -> String
fromLocation (Location char int) = char : show int

-- feedback function takes a target and a guess computes the feedback based on the exact matches and proximity of guessed locations against target locations.
-- it gives back a tuple with 3 elements - (exacts, ones, twos).
-- Where exacts denotes the total number of correct guesses; ones denotes total 
-- number of guesses that are 1 distance away from the target; twos denotes 
-- the total number of guesses that are 2 distance away from the target.
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback targets guess = (exacts, ones, twos)
  where
    -- Convert char to integer
    colToInt :: Char -> Int
    colToInt char = fromEnum char - fromEnum 'A' + 1

    -- Calculate distance between two Locations
    distance :: Location -> Location -> Int
    distance (Location c1 r1) (Location c2 r2) =
      max (abs (colToInt c1 - colToInt c2)) (abs (r1 - r2))

    -- Calculate minimum distance to any target for each guessed location
    counts :: [Int]
    counts = map (\x -> minimum (map (distance x) targets)) guess

    -- Count exact matches (0 distance) and near misses (distances 1 and 2).
    exacts, ones, twos :: Int
    exacts = length (filter (== 0) counts)
    ones = length (filter (== 1) counts)
    twos = length (filter (== 2) counts)

-- Provide an initial guess and initial game state.
-- initialGuess fuction returns a tuple of a list of Locations and a GameState.
initialGuess :: ([Location], GameState)
initialGuess = (init, gameState)
  where
    -- Initial guess points to the middle of the grid and the top corners, aiming to cover as much of the grid as possible.
    init = [Location 'A' 2, Location 'D' 3, Location 'H' 2]
    gameState = allGameState init

-- allGameState function generates all possible game states, given an initial guess.                     
allGameState :: [Location] -> GameState
allGameState init = [[Location a b, Location c d, Location e f] |
                       a <- letters,
                       b <- nums,
                       c <- letters,
                       d <- nums,
                       (a, b) < (c, d),
                       e <- letters,
                       f <- nums,
                       (c, d) < (e, f)]
  where
    -- Define grid dimensions.
    letters = ['A' .. 'H']
    nums = [1 .. 4]
    initSet = Set.fromList init
    availableLocs = [(l, n) | l <- letters, n <- nums, notElemSet (Location l n) initSet]
    notElemSet = Set.notMember

-- nextGuess function takes a previous guess and feedback and returns the next guess and updated game state. 
nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> ([Location], GameState)
nextGuess (preGuess, preGameState) preFeedback = (newGuess, nextGameState)
  where
    updatedGameStates = updateGameState preGameState preGuess preFeedback
    nextGameState = filter (/= preGuess) updatedGameStates
    newGuess = bestGuess nextGameState

-- updateGameState function updates the game state based on the feedback.
updateGameState :: GameState -> [Location] -> (Int,Int,Int) -> GameState
updateGameState [] _ _ = []
updateGameState (x:xs) prevGuess fb
    | feedback x prevGuess == fb =
        x:updateGameState xs prevGuess fb
    | otherwise = updateGameState xs prevGuess fb

-- bestGuess function returns the best guess based on the expected value of each possible game state.
bestGuess :: GameState -> [Location]
bestGuess gameState = fst (head bestGuess)
    where
        expectedValueList = map (`calculateExpectation` gameState) gameState
        stateValueTuple    = zip gameState expectedValueList
        bestGuess         = sortBy (flip compare `on` snd) stateValueTuple

sortTuple :: (Ord b) => (a, b) -> (a, b) -> Ordering
sortTuple x y =
    if snd x < snd y
    then GT
    else LT

-- calculateExpectation function calculates the expected value of a guess based on the remaining game states.
calculateExpectation :: [Location] -> GameState -> Double
calculateExpectation guess gameState = sum (zipWith (/) squaredOccurrenceList totals)
  where
    stateFeedbacks = [feedback restGameState guess | restGameState <- delete guess gameState]
    groupedList = group . sort $ stateFeedbacks
    frequencyList = map length groupedList
    n = length groupedList
    -- Calculate the squared occurrences and total occurrences for each feedback.
    squaredOccurrenceList = map ((^2) . fromIntegral) frequencyList
    totals = repeat (fromIntegral $ sum frequencyList)
