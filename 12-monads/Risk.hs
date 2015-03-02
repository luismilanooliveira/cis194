{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List (sortBy, genericLength)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int
type BattleResult = (Army, Army)

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
              let nAt = min 3 (attackers bf - 1)
              let nDf = max 2 $ defenders bf
              atDies <- replicateM nAt die
              dfDies <- replicateM nDf die
              let att = sortBy (flip compare) $ map unDV atDies
              let def = sortBy (flip compare) $ map unDV dfDies
              let (a, d) = process $ zip att def
              return (Battlefield (attackers bf - a) (defenders bf - d))

process :: [(Int, Int)] -> BattleResult
process = foldr go (0, 0)
  where go (f, s) (r', r'') = if f > s then (r' + 1, r'') else (r', r'' + 1)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf < 2 || defenders bf < 1 = return bf
  | otherwise                            = battle bf >>= invade

attackerVictory :: Battlefield -> Bool
attackerVictory = (<= 0) . defenders

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  result <- replicateM 1000 (invade bf)
  return $ (/1000.00) . genericLength . filter attackerVictory $ result
