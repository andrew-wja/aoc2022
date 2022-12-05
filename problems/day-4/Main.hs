{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.IO (readFile)
import System.Environment (getArgs)
import Data.Set (Set, isSubsetOf, intersection, null, fromList)
import Data.List.Split (splitOn)

parseAssignment :: String -> (Set Int, Set Int)
parseAssignment line =
  let [a, b] = splitOn "," line
      [startA, endA] :: [Int] = map read $ splitOn "-" a
      [startB, endB] :: [Int] = map read $ splitOn "-" b
      setA = fromList $ take (endA - startA + 1) $ iterate (+1) startA
      setB = fromList $ take (endB - startB + 1) $ iterate (+1) startB
  in (setA, setB)

main = do
  args <- getArgs
  contents <- readFile (args !! 0)

  let assignments = map parseAssignment $ filter (not . Prelude.null) $ lines contents
  let redundancies = filter (\(elfA, elfB) -> elfA `isSubsetOf` elfB || elfB `isSubsetOf` elfA) assignments

  putStrLn "Part 1:"
  putStrLn $ show $ length redundancies

  let overlaps = filter (\(elfA, elfB) -> not $ Data.Set.null $ elfA `intersection` elfB) assignments

  putStrLn "Part 1:"
  putStrLn $ show $ length overlaps

  return ()
