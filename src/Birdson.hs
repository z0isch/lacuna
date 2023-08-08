{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Birdson where

import Codec.Picture
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
    gets,
    modify,
  )
import System.Random (randomRIO)

data Birdson = Birdson
  { birdsonActive :: Set (V2 Int),
    birdsonInactive :: Set (V2 Int),
    birdsonKDT :: KdTree Int (V2 Int),
    birdsonRadius :: Int,
    birdsonCandidates :: Int,
    birdsonBounds :: V2 Int
  }
  deriving stock (Show)

initialBirdson :: Birdson
initialBirdson =
  Birdson
    { birdsonActive = Set.singleton (V2 0 0),
      birdsonInactive = mempty,
      birdsonKDT = KDT.singleton (\(V2 a b) -> [a, b]) (V2 0 0),
      birdsonRadius = 75,
      birdsonCandidates = 10,
      birdsonBounds = V2 500 500
    }

birdson :: MonadIO m => StateT Birdson m ()
birdson = do
  active <- randomEl =<< gets birdsonActive
  candidates <- randomInAnnulus active =<< get
  kdt <- gets birdsonKDT
  radius <- gets birdsonRadius
  case L.dropWhile ((<= int2Double radius) . snd) $ (\v -> (v, distanceToNearestNeighbor kdt v)) <$> candidates of
    ((c, _) : _) -> modify $ \b ->
      b
        { birdsonKDT = KDT.insert (birdsonKDT b) c,
          birdsonActive = Set.insert c (birdsonActive b)
        }
    _ -> modify $ \b ->
      b
        { birdsonActive = Set.delete active (birdsonActive b),
          birdsonInactive = Set.insert active (birdsonInactive b)
        }
  whenM (not . Set.null <$> gets birdsonActive) birdson
  pure ()

randomEl :: MonadIO f => Set a -> f a
randomEl s = (`Set.elemAt` s) <$> randomRIO (0, Set.size s - 1)

inBounds :: V2 Int -> V2 Int -> Bool
inBounds (V2 bx by) (V2 x y) = x <= bx && x >= -by && y <= by && y >= -by

randomInAnnulus :: MonadIO m => V2 Int -> Birdson -> m [V2 Int]
randomInAnnulus around Birdson {..} =
  L.filter (inBounds birdsonBounds)
    <$> for
      [0 .. birdsonCandidates]
      ( \_ -> do
          d <- randomRIO (birdsonRadius, birdsonRadius * 2)
          a <- fmap round . angle <$> randomRIO (0, 2 * pi @Double)
          pure $ around ^+^ (d *^ a)
      )

distanceToNearestNeighbor :: KdTree Int (V2 Int) -> V2 Int -> Double
distanceToNearestNeighbor kdt v = distance (int2Double <$> KDT.nearest kdt v) (int2Double <$> v)

debugImage :: Birdson -> Image PixelRGB8
debugImage Birdson {..} = generateImage pixelRenderer bx by
  where
    pixelRenderer :: Int -> Int -> PixelRGB8
    pixelRenderer px py
      | Set.member (V2 (px - bx) (py - by)) birdsonActive = PixelRGB8 100 100 100
      | Set.member (V2 (px - bx) (py - by)) birdsonInactive = PixelRGB8 255 255 255
      | otherwise = PixelRGB8 0 0 0
    V2 bx by = birdsonBounds