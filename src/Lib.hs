{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

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
      sumAdd,
      manySums,
      sumDice,
      sumDice2,
      zipInfWith,
      foldrList,
      helper,
      moreDice,
      randomRs,
      lotsOfDice,
      randomRsLists,
      goTwoDeep,
      general,
      faceNumberRollsPlus,
      faceNumberRolls,
      faceNumber,
      faceNumberPlus,
      faceRolls,
      faceRollsPlus,
      main5
    ) where

import System.Random
import Control.Monad.Random
import System.Environment (getArgs)
import qualified System.Exit as SE
import Data.Binary (encodeFile, decodeFile)
import qualified Data.Foldable as F (fold)

--main5 = do
--  (command:args) <- getArgs
--  n <- getIntArg args
--  let (Just action) = lookup command dispatch
--  action n

getIntArg :: IO Int
getIntArg = fmap (read . head) getArgs
  
main5 = getArgs >>= parse >>= putStr . tac

dispatch :: [(String, Integer -> Integer -> Integer -> Integer -> IO ())]
dispatch = [("faceNumberRollsPlus", faceNumberRollsPlus)]  

tac = unlines . reverse . lines
parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse ["faceNumberRollsPlus"] = faceNumberRollsPlus (8 :: Integer) (2 :: Integer) (3 :: Integer) (0 :: Integer) >> exit
--parse (x:xs)
--  | x == "faceNumberRollsPlus"
parse [] = getContents
parse fs = concat `fmap` mapM readFile fs


usage = putStrLn "Usage: faceNumberRollsPlus ?dX Xd? numberOfRolls modifier\n       faceNumberRolls ?dX Xd? numberOfRolls\n       faceNumber ?dX Xd?\n       faceNumberPlus ?dX Xd? modifier\n       faceRolls ?dX numberOfRolls\n       faceRollsPlus ?dX numberOfRolls modifier"

version = putStrLn "Haskell diceCalc 0.1"
exit = SE.exitWith SE.ExitSuccess
die = SE.exitWith (SE.ExitFailure 1)

  

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

mydie :: (RandomGen g) => Int -> Rand g Int
mydie x = getRandomR (1,x)

dice :: (RandomGen g) => Int -> Int -> Rand g [Int]
dice n d = sequence (replicate n $ mydie d)

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

replace :: Int -> a -> Int
replace x _ = x

specialHead :: Num a => [a] -> a
specialHead [] = 0
specialHead (x:xs) = x



--  let
--    allHeads = mconcat $ fmap (take 1) xs
--    rest = fmap (drop 1) xs
--    summed = (sum allHeads):[]
--  in
--    summed ++ (foldrList rest)
--    allHeads

    
zipInfWith :: (Int -> Int -> Int) -> [[Int]] -> [[Int]]
zipInfWith f [[]] = []
zipInfWith f [] = []
zipInfWith f xs =
  let
    allHeads = fmap ((:[]) . head) xs
--    val = (foldr (+) 0 allHeads)
--    rest = fmap (drop 1) xs
  in
    allHeads
--    [val]
-- :(zipInfWith f rest)
    
sumDice2 :: Int -> Int -> IO ()
sumDice2 d n = do
  x <- newStdGen
  print $ take n (zipWith (+) ((randomRs (1,d) x) :: [Int]) ((randomRs (1,d) x):: [Int]))

manySums :: (Int -> Int -> IO ()) -> Int -> Int -> Int -> IO ()
manySums f faces number rolls = do
  x <- f faces number
--  let list = replicate x [1..rolls]
  print $ x


foldrList :: [[Int]] -> [Int]
foldrList [] = []
foldrList xs = helper (length (head xs)) xs

helper :: Int -> [[Int]] -> [Int]
helper 0 _ = []
helper x xs = (sum $ mconcat $ fmap (take 1) xs):(helper (x - 1) (fmap (drop 1) xs))

sumDice :: Int -> Int -> IO ()
sumDice d n = do
  x <- newStdGen
  y <- newStdGen
  print $ take n (zipWith (+) ((randomRs (1,d) x) :: [Int]) ((randomRs (1,d) y):: [Int]))



--moreDice :: Int -> IO ()
moreDice sides number = do
  first <- sumDice sides number
  second <- sumDice sides number
  print $ [first,second]

instance Monoid Integer where
  mempty = 0
  mconcat = foldr (+) 0

instance Semigroup Integer where
  (<>) = (+)

lotsOfDice sides number = do
  x <- newStdGen
  print $ take number $ ((randomRs (1,sides) x) :: [Int])

integerTake :: Integer -> [a] -> [a]
integerTake _ [] = []
integerTake 0 xs = []
integerTake int (x:xs) = x:(integerTake (int - 1) xs)

randomRsLists :: (Eq b,Num b,
                  System.Random.Random a,System.Random.RandomGen g) =>
                 (a,a) ->
                 Integer ->
                 b ->
                 g ->
                 [[a]]
randomRsLists (_,_) _ 0 _ = []
randomRsLists (mi,ma) number rolls geni =
  let
    firstCase = (integerTake number $ randomRs (mi,ma) geni)
    recursiveCase = (randomRsLists (mi,ma) number (rolls - 1) (snd (split geni)))
  in
    firstCase:recursiveCase

listAndSum :: (Foldable t, Monoid a) => t a -> (a, t a)
listAndSum x = (F.fold x,x)

goTwoDeep :: Num a => (a -> a) -> [[a]] -> [[a]]
goTwoDeep _ [] = []
goTwoDeep f (x:xs) = (fmap f x):(goTwoDeep f xs)

general mi ma number rolls plus = do
  x <- newStdGen
  let myList1 = randomRsLists (mi,ma) number rolls x
  let myList2 = goTwoDeep (+ plus) myList1
  let mySumAndList = fmap listAndSum myList2
  print $ mySumAndList




faceNumberRollsPlus :: Integer -> Integer -> Integer -> Integer -> IO ()
faceNumberRollsPlus ma number rolls plus = general 1 ma number rolls plus

faceNumberRolls :: Integer -> Integer -> Integer -> IO ()
faceNumberRolls ma number rolls = general 1 ma number rolls 0

faceNumber :: Integer -> Integer -> IO ()
faceNumber ma number = general 1 ma number 1 0

faceNumberPlus :: Integer -> Integer -> Integer -> IO ()
faceNumberPlus ma number plus = general 1 ma number 1 plus

faceRolls :: Integer -> Integer -> IO ()
faceRolls ma rolls = general 1 ma 1 rolls 0

faceRollsPlus :: Integer -> Integer -> Integer -> IO ()
faceRollsPlus ma rolls plus = general 1 ma 1 rolls plus
