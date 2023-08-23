module CanvasPoint (module CanvasPoint) where

import Data.Coerce (coerce)
import Import
import Lacuna qualified
import Linear (Additive, V2 (V2))
import Linear.Metric (Metric)

newtype CanvasPoint a = CanvasPoint (V2 a)
  deriving stock (Eq, Functor, Show)
  deriving newtype (Num, Additive, Metric, Ord)

mkCanvasPointUnsafe :: V2 a -> CanvasPoint a
mkCanvasPointUnsafe = CanvasPoint

toCanvasPoint :: Num a => V2 a -> CanvasPoint a
toCanvasPoint (V2 x y) = CanvasPoint $ V2 (x + 800) (y + 450)

fromCanvasPoint :: Num a => CanvasPoint a -> V2 a
fromCanvasPoint (CanvasPoint (V2 x y)) = V2 (x - 800) (y - 450)

toDoubleCanvasPoint :: Integral a => CanvasPoint a -> CanvasPoint Double
toDoubleCanvasPoint (CanvasPoint p) = CanvasPoint $ fromIntegral <$> p

lineToPointQuadrance :: forall a. Fractional a => (CanvasPoint a, CanvasPoint a) -> CanvasPoint a -> a
lineToPointQuadrance = coerce (Lacuna.lineToPointQuadrance @a)

inBoundingBox :: forall a. (Ord a) => (CanvasPoint a, CanvasPoint a) -> CanvasPoint a -> Bool
inBoundingBox = coerce (Lacuna.inBoundingBox @a)

toTuple :: CanvasPoint a -> (a, a)
toTuple (CanvasPoint (V2 x y)) = (x, y)