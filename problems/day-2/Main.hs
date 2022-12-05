{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.IO (readFile)
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data Shape = Rock | Paper | Scissors deriving (Eq, Ord, Enum, Show)

data Outcome = Loss | Draw | Win deriving (Eq, Ord, Enum, Show)

parseOpponentShape :: String -> Maybe Shape
parseOpponentShape move
  | "A" <- move = Just Rock
  | "B" <- move = Just Paper
  | "C" <- move = Just Scissors
  | otherwise = Nothing

parseOurShape :: String -> Maybe Shape
parseOurShape move
  | "X" <- move = Just Rock
  | "Y" <- move = Just Paper
  | "Z" <- move = Just Scissors
  | otherwise = Nothing

parseRound :: String -> Maybe (Shape, Shape)
parseRound line = do
  let [opponent, us] = splitOn " " line
  opponent' <- parseOpponentShape opponent
  us' <- parseOurShape us
  return (opponent', us')

scoreOutcome :: Outcome -> Int
scoreOutcome Loss = 0
scoreOutcome Draw = 3
scoreOutcome Win  = 6

scoreShape :: Shape -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

outcome :: (Shape, Shape) -> Outcome
outcome throws
  | (Rock, Rock) <- throws = Draw
  | (Rock, Paper) <- throws = Win
  | (Rock, Scissors) <- throws = Loss
  | (Paper, Rock) <- throws = Loss
  | (Paper, Paper) <- throws = Draw
  | (Paper, Scissors) <- throws = Win
  | (Scissors, Rock) <- throws = Win
  | (Scissors, Paper) <- throws = Loss
  | (Scissors, Scissors)  <- throws = Draw

scoreRound :: (Shape, Shape) -> Int
scoreRound throws =
  let oc = outcome throws
  in (scoreOutcome oc + (scoreShape $ snd throws))

-- Part 2 logic: define a reverse lookup that projects an outcome to the desired shape

outcomeForShape :: Shape -> Outcome
outcomeForShape Rock = Loss
outcomeForShape Paper = Draw
outcomeForShape Scissors = Win

shapeForOutcome :: Shape -> Outcome -> Shape
shapeForOutcome opponent oc
  | (Rock, Loss) <- (opponent, oc) = Scissors
  | (Rock, Draw) <- (opponent, oc) = Rock
  | (Rock, Win) <- (opponent, oc) = Paper
  | (Paper, Loss) <- (opponent, oc) = Rock
  | (Paper, Draw) <- (opponent, oc) = Paper
  | (Paper, Win) <- (opponent, oc) = Scissors
  | (Scissors, Loss) <- (opponent, oc) = Paper
  | (Scissors, Draw) <- (opponent, oc) = Scissors
  | (Scissors, Win) <- (opponent, oc) = Rock

remapShape :: (Shape, Shape) -> Shape
remapShape (opponent, us) =
  let desiredOutcome = outcomeForShape us
  in shapeForOutcome opponent desiredOutcome

main = do
  args <- getArgs
  contents <- readFile (args !! 0)

  let rounds = map (fromJust . parseRound) $ lines contents
  let scores = map scoreRound rounds

  putStrLn "Part 1:"
  putStrLn $ show $ sum scores

  let rounds' = map (\(o, u) -> (o, remapShape (o, u))) rounds
  let scores' = map scoreRound rounds'
  putStrLn "Part 2:"
  putStrLn $ show $ sum scores'
  return ()
