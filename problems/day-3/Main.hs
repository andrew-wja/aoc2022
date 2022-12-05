{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.IO (readFile)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.Char (ord, isAsciiUpper, isAsciiLower)
import Data.Set (Set, elems, intersection, fromList)

splitCompartments :: String -> (String, String)
splitCompartments sack = splitAt (length sack `div` 2) sack

findDuplicate :: (String, String) -> Char
findDuplicate (a, b) =
  let a' :: Set Char = fromList a
      b' :: Set Char = fromList b
  in head $ elems $ intersection a' b'

priority :: Char -> Maybe Int
priority c
  | isAsciiLower c = Just $ (ord c) - 96
  | isAsciiUpper c = Just $ 27 + ((ord c) - 65)
  | otherwise = Nothing

-- Part 2 logic

selectGroups :: [String] -> [[String]]
selectGroups [] = []
selectGroups rabble =
  let (group, rest) = splitAt 3 rabble
  in (group:(selectGroups rest))

findBadge :: [String] -> Char
findBadge group =
  let [a, b, c] :: [Set Char] = map fromList group
  in head $ elems $ a `intersection` b `intersection` c

main = do
  args <- getArgs
  contents <- readFile (args !! 0)

  let backpacks = filter (not . null) $ lines contents
  let dupes = map (findDuplicate . splitCompartments) backpacks
  let priorities = map (fromJust . priority) dupes

  putStrLn "Part 1:"
  putStrLn $ show $ sum priorities

  let badges = map findBadge $ selectGroups backpacks
  let badgePriorities = map (fromJust . priority) badges

  putStrLn "Part 2:"
  putStrLn $ show $ sum badgePriorities

  return ()
