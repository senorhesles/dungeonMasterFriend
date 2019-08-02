module Lib
    ( someFunc, Die, intToDie, rollDieThreeTimes, rollDieXTimes, rollDice,
      randomRIO,
      rollDice2,
      rollDice3,
      getStdGen,
      newStdGen,
      rollDSidedDiceNTimes,
      play,
      evalRandIO,
      sumDSidedDiceNTimes,
      sumAdd
    ) where

import System.Random
import Control.Monad.Random
import System.IO
import Data.Char (toUpper)
import System.Directory (renameFile, getTemporaryDirectory)
import System.Environment (getArgs)
import Data.Binary (encodeFile, decodeFile)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

type Dice = Int

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x ->
      error $
      "intToDie got non 1-6 integer: "
      ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1,6) s :: (Int,StdGen)
      (d2, s2) = randomR (1,6) s1 :: (Int, StdGen)
      (d3, _) = randomR (1,6) s2
  (intToDie d1, intToDie d2, intToDie d3)

-- x is the number of dice
-- y is randomly generated
rollDieXTimes :: Int -> Int -> [Int]
rollDieXTimes x y = do
  let
    s = mkStdGen y
    (d1, s1) = randomR (1,x) s :: (Int,StdGen)
    (d2, s2) = randomR (1,x) s1 :: (Int, StdGen)
    (d3, _) = randomR (1,x) s2
    in
    [d1,d2,d3]

rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))

rollDice2 :: Int -> IO Int
rollDice2 x = randomRIO (1,x)

-- x is the number of dice
-- y is the arity of the dice
rollDice3 :: Int -> Int -> [IO Int]
rollDice3 x y
  | x > 0 = (rollDice2 y):(rollDice3 (x - 1) y)
  | otherwise = []

die :: (RandomGen g) => Int -> Rand g Int
die x = getRandomR (1,x)

dice :: (RandomGen g) => Int -> Int -> Rand g [Int]
dice n d = sequence (replicate n $ die d)

play :: Int -> Int -> IO ()
play n d  = do
    values <- evalRandIO $ dice n d
    putStrLn $ show values

rollDSidedDiceNTimes :: Int -> Int -> IO ()
rollDSidedDiceNTimes d n = do
  x <- newStdGen
  print $ take n ((randomRs (1,d) x) :: [Int])

rollDSidedDiceNTimesAddP :: Int -> Int -> Int -> IO ()
rollDSidedDiceNTimesAddP d n p = do
  x <- newStdGen
  print $ fmap (+ p) $ take n ((randomRs (1,d) x) :: [Int])


sumDSidedDiceNTimes :: Int -> Int -> IO ()
sumDSidedDiceNTimes d n = do
  x <- newStdGen
  print $ sum $ take n ((randomRs (1,d) x) :: [Int])

-- sumAdd :: Int -> Int -> Int -> IO ()
-- sumAdd d n p = do
--   x <- newStdGen
--   let list = take n ((randomRs (1,d) x) :: [Int])
--   print $ list
--   let newContents = take n ((randomRs (1,d) x) :: [Int])
--   when (length newContents > 0) $
--     writeFile "file.txt" (show newContents)

sumAdd :: Int -> Int -> Int -> IO ()
sumAdd d n p = do
  x <- newStdGen
  let list = take n ((randomRs (1,d) x) :: [Int])
  print $ list
  encodeFile "/home/mario/workspace/dungeonMasterFriend/file.txt" list
--  let newContents = take n ((randomRs (1,d) x) :: [Int])
--  when (length newContents > 0) $
--    writeFile "file.txt" (show newContents)


