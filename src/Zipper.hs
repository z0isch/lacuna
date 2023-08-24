module Zipper (Zipper, zipper, lefts, rights) where

import Control.Comonad.Store (Comonad (..), ComonadStore (..))
import Import hiding (lefts, rights)
import RIO.List qualified as L
import RIO.Seq qualified as Seq

data Zipper t a = Zipper (forall b. Seq b -> t b) {-# UNPACK #-} !Int !(Seq a)

instance ComonadStore Int (Zipper t) where
  pos (Zipper _ i _) = i
  peek j (Zipper _ _ s) = Seq.index s j
  experiment f (Zipper _ i s) = Seq.index s <$> f i

instance Functor (Zipper t) where
  fmap f (Zipper t i s) = Zipper t i (fmap f s)

instance Foldable (Zipper t) where
  foldMap f (Zipper _ _ s) = foldMap f s

instance Traversable (Zipper t) where
  traverse f (Zipper t i s) = Zipper t i <$> traverse f s

instance Comonad (Zipper t) where
  extend f (Zipper t i s) = Zipper t i (Seq.mapWithIndex (\j _ -> f (Zipper t j s)) s)
  extract (Zipper _ i s) = Seq.index s i

zipper :: Traversable t => t a -> Maybe (Zipper t a)
zipper t = case toList t of
  [] -> Nothing
  xs -> Just (Zipper (refill t) 0 (Seq.fromList xs))
  where
    refill bs as = snd (L.mapAccumL (\(a : as') _ -> (as', a)) (toList as) bs)

extractAllComparing :: (Int -> Int -> Bool) -> Zipper f a -> [a]
extractAllComparing f z = fold $ extend (\z' -> [extract z' | z' `commpareOnPos` z]) z
  where
    commpareOnPos = f `on` pos

lefts :: Zipper f a -> [a]
lefts = extractAllComparing (<)

rights :: Zipper f a -> [a]
rights = extractAllComparing (>)