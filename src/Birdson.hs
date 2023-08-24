{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Birdson (birdson) where

import Control.Lens (use, (%=))
import Control.Lens.TH
import Data.Semigroup (Min (..))
import Import
import Linear
import RIO.List qualified as L
import RIO.Set qualified as Set
import RIO.Set.Partial qualified as Set
import RIO.State
  ( get,
    evalStateT,
  )
import System.Random (Random, randomRIO)

data Birdson = Birdson
  { _birdsonActive :: Set (V2 Int),
    _birdsonInactive :: Set (V2 Int),
    _birdsonRadius :: Int,
    _birdsonCandidates :: Int,
    _birdsonBounds :: Int
  }
  deriving stock (Show)

$(makeLenses ''Birdson)

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
        _birdsonRadius = round $ fromIntegral @_ @Double bounds * 0.2,
        _birdsonCandidates = 25,
        _birdsonBounds = bounds
      }

birdson :: MonadIO m => Int -> m (Set (V2 Int))
birdson = evalStateT (go >> use birdsonInactive) <=< initialBirdson
  where
    go = do
      b <- get
      active <- randomEl $ b ^. birdsonActive
      candidates <- randomInBirdsonAnnulus active b
      let validPoint p =
            inBounds (b ^. birdsonBounds) p
              && quadranceToNearestNeighbor b p >= (b ^. birdsonRadius * b ^. birdsonRadius)
      case L.dropWhile (not . validPoint) candidates of
        (c : _) -> do
          birdsonActive %= Set.insert c
        [] -> do
          birdsonActive %= Set.delete active
          birdsonInactive %= Set.insert active
      unlessM (Set.null <$> use birdsonActive) go

randomEl :: MonadIO f => Set a -> f a
randomEl s = (`Set.elemAt` s) <$> randomRIO (0, Set.size s - 1)

inBounds :: Int -> V2 Int -> Bool
inBounds r v = quadrance v <= r * r

randomInBirdsonAnnulus :: MonadIO m => V2 Int -> Birdson -> m [V2 Int]
randomInBirdsonAnnulus around Birdson {..} = for [0 .. _birdsonCandidates] $ \_ ->
  fmap round
    <$> randomInAnnulus
      (toDouble <$> around)
      ( toDouble _birdsonRadius,
        toDouble $ _birdsonRadius * 2
      )
  where
    toDouble = fromIntegral @_ @Double

type Annulus a = (a, a)

randomInAnnulus :: (Random a, MonadIO m, Floating a) => V2 a -> Annulus a -> m (V2 a)
randomInAnnulus around (r1, r2) = do
  d <- randomRIO (r1, r2)
  a <- angle <$> randomRIO (0, 2 * pi)
  pure $ around + (d *^ a)

quadranceToNearestNeighbor :: Birdson -> V2 Int -> Int
quadranceToNearestNeighbor b p =
  getMin $
    foldMap (Min . qd p) $
      b ^. birdsonActive <> b ^. birdsonInactive