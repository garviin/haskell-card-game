-- File    : Proj1.hs
-- Author  : Garvin Bulkin
-- ID      : 927842
-- Purpose : Project 1 for the subject Declarative Programming (COMP30020).
--
-- This code is made as components for a two player card guessing game.
-- In the game, player 1 picks a series of cards from a standard deck of 52
-- cards. Player 2 will proceed to guess what the cards are. In between
-- guesses, player 1 will give numerical feedback to the guess of player 2.
-- The feedback numbers given mean as follows:
-- 1. The exact cards in the guess that are correct.
-- 2. Amount of cards in the answer that has a lower rank than the guesses.
-- 3. Amount of cards in the answer that has a same rank as the gues cards.
-- 4. Amount of cards in the answer that has a lower suit than the guesses.
-- 5. Amount of cards in the answer that has a same suit as the guess cards.
--
-- The code that follows represent 3 main components of the game.
-- Feedback       : the numerical feedback given to a guess
-- Initial Guess  : the first guess that a player picks.
-- Next Guess     : the guess a player makes after receiving one or more 
--                  feedback after a guess. This utilizes a GameState type,
--                  which keeps track of the pool of possible answers.

module Proj1 (feedback, initialGuess, nextGuess, GameState) where 
import Card
import Data.List

-- Feedback type to improve readability for feedback int tuple
type Feedback = (Int,Int,Int,Int,Int)


-- GameState type keeps track of possible answers in the form of a list 
-- of lists of cards.
type GameState = [[Card]]


-- Returns the aforementioned numerical feedback given after a guess
feedback :: [Card] −> [Card] −> Feedback
feedback answer guess = (exactmatch answer guess, lowerRankCount answer guess,
                         sameRankCount answer guess,
                         higherRankCount answer guess,
                         sameSuitCount answer guess)


-- Function checks how many guess cards exactly matches the answer cards
exactmatch :: [Card] −> [Card] −> Int
-- Uses intersect function to check how many cards are identical
exactmatch answer guess = length (intersect answer guess)


-- Returns the number of cards lower than the lowest answer card
lowerRankCount :: [Card] −> [Card] −> Int
lowerRankCount answer guess
    -- Counts the answer ranks that are lower than the lowest ranked guess card
    = length [rank | (Card suit rank) <− answer, rank < lowestRank] where lowestRank = head (sort (getRanks guess))


-- Extracts the ranks for a list of cards
getRanks :: [Card] −> [Rank]
getRanks [] = []
getRanks ((Card suit rank):cards) = rank : getRanks cards


-- Counts the amount of cards in two card lists that have 
-- each card match is only counted once.
sameRankCount :: [Card] −> [Card] −> Int
    -- Since each card is only counted once, take the minimum sameRankCount answer guess
    = min (countSameRanks answer guess) (countSameRanks guess answer)


-- Returns true if a card has the same rank with at least one card in a given -- list of cards.
rankInGuess :: Card −> [Card] −> Bool
rankInGuess _ [] = False
rankInGuess (Card suit rank) ((Card suitG rankG):xs) 
    | rank == rankG = True
    | otherwise = rankInGuess (Card suit rank) xs


-- Counts the amount of cards in the guess and answer with the same rank
countSameRanks :: [Card] −> [Card] −> Int
countSameRanks [] _ = 0 
countSameRanks (x:xs) ys
    | rankInGuess x ys = 1 + countSameRanks xs ys 
    | otherwise = countSameRanks xs ys


-- Returns the number of cards higher than the highest answer card
higherRankCount :: [Card] −> [Card] −> Int
higherRankCount answer guess
    -- Counts the answer suits that are lower than the lowest suit in the guess
    = length [rank | (Card suit rank) <− answer, rank > highestRank] 
    where highestRank = last (sort (getRanks guess))


-- Counts the amount of cards in two card lists that have the same suits
sameSuitCount :: [Card] −> [Card] −> Int
sameSuitCount answer guess
    = min (countSameSuits answer guess) (countSameSuits guess answer)


-- Returns true if a card has the same rank with at least one card in a given -- list of cards.
suitInGuess :: Card −> [Card] −> Bool
suitInGuess _ [] = False
suitInGuess (Card suit rank) ((Card suit' rank'):xs) 
    | suit == suit' = True
    | otherwise = suitInGuess (Card suit rank) xs


-- Counts the amount of cards in the guess and answer with the same rank
countSameSuits :: [Card] −> [Card] −> Int
countSameSuits [] _ = 0 
countSameSuits (x:xs) guess
    | (suitInGuess x guess) = 1 + countSameSuits xs guess 
    | otherwise = countSameSuits xs guess


-- Returns the initial gamestate and initial guess.
-- The initial guess made is the middle element in the answerpool. initialGuess :: Int −> ([Card], GameState)
initialGuess n
    = (initialGuess, possibleAnswers)
    where possibleAnswers = generateCombinations n initState
          initialGuess = mid possibleAnswers


-- Returns a list of all the 52 cards to generate all possible answers
initState :: [Card]
initState = [(minBound :: Card)..(maxBound :: Card)]


-- Extracts the middle element from a list of lists of cards.
mid :: [[Card]] −> [Card]
mid cards = cards !! middleIndex
    where middleIndex = div (length cards) 2


-- Function generates a list of all possible n combinations for a list of cards
generateCombinations :: Int −> [Card] −> [[Card]]
generateCombinations 0 _ = [[]]
-- Recursively generates the permutation of n cards from the list of cards 
-- with a recursive list comprehension.
generateCombinations n possibleAnswers
    = [card:cards |
       card:cardss <− tails possibleAnswers, 
       cards <− generateCombinations (n−1) cardss]


-- The function has 2 main functionalities:
-- 1. Removes answers that has different feedback values from the answer pool 
-- 2. Returns a new guess, which is the middle element from the new answer pool 
nextGuess :: ([Card],GameState) −> Feedback −> ([Card],GameState)
nextGuess (guess, answerPool) feedback
    = (newGuess, newAnswerPool)
    where newAnswerPool = pruneAnswers guess feedback answerPool
          newGuess = mid newAnswerPool


-- Removes possible answers that does not have a feedback equal to the feedback -- of the guess.
pruneAnswers :: [Card] −> Feedback −> [[Card]] −> [[Card]]
pruneAnswers prevGuess guessFeedback possibleAnswers
    -- Uses filter functino to prune the answerpool
    = filter
      -- Inline function checks if an element in the answerpool has the same 
      -- feedback with the previous guess
      (\answers -> (guessFeedback == feedback answers prevGuess)) possibleAnswers