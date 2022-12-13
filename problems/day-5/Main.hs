{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.IO (readFile)
import System.Environment (getArgs)
import Data.Map (Map)
import Data.List (transpose, null, replicate)
import Data.Char (isDigit, isAlpha, isSpace)
import Data.List.Split (chunksOf, split, keepDelimsR, whenElt)

type CrateStack = [Maybe Char]

data Action = Move { n :: Int
                   , from :: Int
                   , to :: Int
                   } deriving Show

-- Some assumptions: crates are always identified by a single character
-- in square brackets, meaning the width of a crate stack is always 3.
-- Stack labels are always single numeric digits. This limits the number
-- of stacks to 10, though that fact is not assumed here.
parseProblem :: [String] -> [(Int, CrateStack)]
parseProblem lines =
    let (stackContents, rest) = break (all (\x -> isDigit x || isSpace x)) lines
        stackLabels = head rest
        actionDescriptions = filter (not . null) $ tail rest
        stackLabelStrings :: [String] = filter (not . null) $ map (filter isDigit) $ split (keepDelimsR $ whenElt isDigit) stackLabels
        numStacks = length stackLabelStrings
        transposedStacks = map (parseRow numStacks) stackContents
    in zip (map read stackLabelStrings) $ transpose transposedStacks
    where
      parseRow numStacks rowText =
        let items = chunksOf 4 rowText
        in
          if (length items) < numStacks then
            -- we can only have empty cells at the end
            let numEmptyCells = numStacks - (length items)
            in (map parseCrate items) ++ (replicate numEmptyCells Nothing)
          else map parseCrate items

      parseCrate crateText =
        let crateLabel = filter isAlpha crateText
        in
            case (length crateLabel) of
              1 -> Just $ head crateLabel
              _ -> Nothing

type ProblemState = Map Int CrateStack

main = do
  args <- getArgs
  contents <- readFile (args !! 0)

  let parsedProblem = parseProblem $ lines contents

  putStrLn $ show parsedProblem

  return ()