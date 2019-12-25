module Main where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.List                      ( intercalate )
import           Text.Printf
import           Control.Applicative

main :: IO ()
main = putStrLn $ intercalate "\n" $ solution ()


newtype Diff = Diff (Int, Int) deriving (Show, Eq)

sumDiff :: Diff -> Diff -> Diff
sumDiff (Diff (x, y)) (Diff (x', y')) = Diff (x + x', y + y')

digitSegmentsDict :: Map Char [Int]
digitSegmentsDict = Map.fromList
  [ ('0', [1, 1, 1, 1, 1, 1, 0])
  , ('1', [0, 1, 1, 0, 0, 0, 0])
  , ('2', [1, 1, 0, 1, 1, 0, 1])
  , ('3', [1, 1, 1, 1, 0, 0, 1])
  , ('4', [0, 1, 1, 0, 0, 1, 1])
  , ('5', [1, 0, 1, 1, 0, 1, 1])
  , ('6', [1, 0, 1, 1, 1, 1, 1])
  , ('7', [1, 1, 1, 0, 0, 0, 0])
  , ('8', [1, 1, 1, 1, 1, 1, 1])
  , ('9', [1, 1, 1, 1, 0, 1, 1])
  ]

rng :: [Int]
rng = [1 .. 100]

variants :: [String]
variants =
  let numbers = [ (a, b, a * b) | a <- rng, b <- rng ]
      toFormattedString (a, b, c) = printf "%02d%02d%04d" a b c
      strings = map toFormattedString numbers
  in  strings

diffDigits :: Int -> Int -> Diff -> Diff
diffDigits a b (Diff (ins, del)) | a > b     = Diff (ins + 1, del)
                                 | a < b     = Diff (ins, del + 1)
                                 | otherwise = Diff (ins, del)

diffChars :: Char -> Char -> Maybe Diff
diffChars a b =
  let getCharSegments = flip Map.lookup digitSegmentsDict
      segmentsToDiff  = case map getCharSegments [a, b] of
        [Just a', Just b'] -> Just (zip a' b')
        _                  -> Nothing
      runDiff = foldr (uncurry diffDigits) (Diff (0, 0))
  in  fmap runDiff segmentsToDiff

diffNumbers :: String -> String -> Maybe Diff
diffNumbers a b =
  foldr (liftA2 sumDiff) (Just (Diff (0, 0))) $ zipWith diffChars a b

srcNum :: String
srcNum = "25691725"

diffsWithFiveSegments :: String -> String -> Bool
diffsWithFiveSegments a b = diffNumbers a b == Just (Diff (5, 5))

solution :: () -> [String]
solution () = filter (diffsWithFiveSegments srcNum) variants
