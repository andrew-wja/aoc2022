{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.IO (readFile)
import System.Environment (getArgs)
import Data.List (span, null, sort)

-- Break up a list of lines into chunks separated by the empty lines
chunk [] = []
chunk lines =
  let (piece, rest) = span (not . null) lines
  in (piece:(chunk $ drop 1 rest))

main = do
  args <- getArgs
  contents <- readFile (args !! 0)
  let chunks = chunk $ lines contents
  let chunksOfInts :: [[Int]] = map (map read) chunks
  let summedChunks = map sum chunksOfInts
  putStrLn "Part 1:"
  putStrLn $ show $ maximum summedChunks
  let sortedTotals = reverse $ sort summedChunks -- sort defaults to ascending order
  let top3 = take 3 sortedTotals
  putStrLn "Part 2:"
  putStrLn $ show $ sum top3
  return ()
