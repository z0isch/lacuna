{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Birdson (runBirdson) where

import Control.Lens (use, (%=))
import Control.Lens.TH
import Data.KdTree.Dynamic (KdTree)
import Data.KdTree.Dynamic qualified as KDT
import GHC.Float (int2Double)
import Import
import Linear
import RIO.List qualified as L
import RIO.Set qualified as Set
import RIO.Set.Partial qualified as Set
import RIO.State
  ( MonadState (get),
    StateT,
    evalStateT,
  )
import System.Random (randomRIO)

data Birdson = Birdson
  { _birdsonActive :: Set (V2 Int),
    _birdsonInactive :: [V2 Int],
    _birdsonKDT :: KdTree Int (V2 Int),
    _birdsonRadius :: Int,
    _birdsonCandidates :: Int,
    _birdsonBounds :: Int
  }
  deriving stock (Show)

$(makeLenses ''Birdson)

runBirdson :: MonadIO m => Int -> m [V2 Int]
runBirdson = evalStateT birdson <=< initialBirdson

initialBirdson :: MonadIO m => Int -> m Birdson
initialBirdson bounds = do
  let getInitial = do
        v <- V2 <$> randomRIO (-bounds, bounds) <*> randomRIO (-bounds, bounds)
        if inBounds bounds v then pure v else getInitial
  initial <- getInitial
  pure $
    Birdson
      { _birdsonActive = Set.singleton initial,
        _birdsonInactive = mempty,
        _birdsonKDT = KDT.singleton (\(V2 a b) -> [a, b]) initial,
        _birdsonRadius = round $ fromIntegral @_ @Double bounds * 0.2,
        _birdsonCandidates = 25,
        _birdsonBounds = bounds
      }

birdson :: MonadIO m => StateT Birdson m [V2 Int]
birdson = go >> use birdsonInactive
  where
    go = do
      b <- get
      active <- randomEl $ b ^. birdsonActive
      candidates <- randomInAnnulus active b
      let validPoint p =
            inBounds (b ^. birdsonBounds) p
              && distanceToNearestNeighbor (b ^. birdsonKDT) p >= int2Double (b ^. birdsonRadius)
      case L.dropWhile (not . validPoint) candidates of
        (c : _) -> do
          birdsonKDT %= (`KDT.insert` c)
          birdsonActive %= Set.insert c
        [] -> do
          birdsonActive %= Set.delete active
          birdsonInactive %= (active :)
      unlessM (Set.null <$> use birdsonActive) go

randomEl :: MonadIO f => Set a -> f a
randomEl s = (`Set.elemAt` s) <$> randomRIO (0, Set.size s - 1)

inBounds :: Int -> V2 Int -> Bool
inBounds r (V2 x y) = (x * x + y * y) <= r * r

randomInAnnulus :: MonadIO m => V2 Int -> Birdson -> m [V2 Int]
randomInAnnulus around Birdson {..} =
  for
    [0 .. _birdsonCandidates]
    ( \_ -> do
        d <- randomRIO (_birdsonRadius, _birdsonRadius * 2)
        a <- fmap round . angle <$> randomRIO (0, 2 * pi @Double)
        pure $ around + (d *^ a)
    )

distanceToNearestNeighbor :: KdTree Int (V2 Int) -> V2 Int -> Double
distanceToNearestNeighbor kdt v = distance (int2Double <$> KDT.nearest kdt v) (int2Double <$> v)
