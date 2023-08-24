{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ViewPatterns #-}

module Lacuna (module Lacuna) where

import Birdson (birdson)
import Control.Lens
import Data.Semigroup (Endo (..), Min (..), Sum (..))
import Import hiding (Lens, Lens', lens, over, to, view, (%~), (.~), (^.), (^..))
import Linear
import RIO.List qualified as L
import RIO.Set qualified as Set
import RIO.State (StateT, gets, modify, evalStateT)
import System.Random (randomRIO)
import qualified RIO.List.Partial as L
import Control.Monad (replicateM)

piece_size :: Int
piece_size = 10

type Flowers = (Set (V2 Int), Set (V2 Int), Set (V2 Int), Set (V2 Int), Set (V2 Int), Set (V2 Int), Set (V2 Int))

instance Semigroup Flowers where
  (a, b, c, d, e, f, g) <> (a', b', c', d', e', f', g') = (a <> a', b <> b', c <> c', d <> d', e <> e', f <> f', g <> g')

data Player = Player1 | Player2
  deriving stock (Show)

data PlayerState = PlayerState
  { _psCaptured :: Flowers,
    _psPawns :: Set (V2 Double)
  }
  deriving stock (Show)

$(makeLenses ''PlayerState)

initialPlayerState :: PlayerState
initialPlayerState = PlayerState {_psCaptured = (mempty, mempty, mempty, mempty, mempty, mempty, mempty), _psPawns = mempty}

data LacunaState = LacunaState
  { _lsFlowers :: Flowers,
    _lsTurn :: Player,
    _lsPlayer1 :: PlayerState,
    _lsPlayer2 :: PlayerState,
    _lsWinner :: Maybe Player
  }
  deriving stock (Show)

$(makeLenses ''LacunaState)

pickRandomEls :: (Ord a, MonadIO m) => Int -> StateT [a] m [a]
pickRandomEls numEls = go numEls []
  where
    go 0 picked = pure picked
    go n picked = gets L.length >>= \case
        0 -> pure picked
        s -> do
          i <- liftIO $ randomRIO (0, s - 1)
          el <- gets (L.!! i)
          modify $ L.delete el 
          go (n-1) $ el:picked
      
initialLacunaState :: forall m. MonadIO m => m LacunaState
initialLacunaState = do
  let _lsTurn = Player1
      _lsPlayer1 = initialPlayerState
      _lsPlayer2 = initialPlayerState
      _lsWinner = Nothing
  _lsFlowers <- initialFlowers
  pure LacunaState {..}
  where
    initialFlowers = do
      b <- liftIO . evalStateT (pickRandomEls 49) . toList =<< birdson 400
      if length b >= 49
        then mkFlowers b
        else initialFlowers

mkFlowers :: MonadIO m => [V2 Int] -> m Flowers
mkFlowers = evalStateT (replicateM 7 (pickRandomEls 7)) >=> \case
    (fmap Set.fromList -> [a,b,c,d,e,f,g]) -> pure (a,b,c,d,e,f,g)
    _ -> error "not enough flowers"

data Move = PlacePawn (V2 Int, V2 Int) (V2 Double)

move :: LacunaState -> Move -> LacunaState
move ls = \case
  PlacePawn (flower1, flower2) pawn ->
    ls
      & addFlowerToPlayer currentPlayer flower1
      & addFlowerToPlayer currentPlayer flower2
      & currentPlayer . psPawns %~ Set.insert pawn
      & lsTurn %~ \case
        Player1 -> Player2
        Player2 -> Player1
      & endGameScoring

addFlowerToPlayer :: ((PlayerState -> Identity PlayerState) -> LacunaState -> Identity LacunaState) -> V2 Int -> LacunaState -> LacunaState
addFlowerToPlayer playerL flower ls =
  ls
    & playerL . psCaptured <>~ flowersToMappend
    & lsFlowers %~ over each (Set.delete flower)
  where
    flowersToMappend = _lsFlowers $ ls & lsFlowers . each %~ mkSet
    mkSet s = fromMaybe mempty $ Set.singleton flower <$ guard (flower `Set.member` s)

currentPlayer :: Lens' LacunaState PlayerState
currentPlayer = lens getter setter
  where
    getter ls =
      ls ^. case ls ^. lsTurn of
        Player1 -> lsPlayer1
        Player2 -> lsPlayer2
    setter :: LacunaState -> PlayerState -> LacunaState
    setter ls ps =
      ls & case ls ^. lsTurn of
        Player1 -> lsPlayer1 .~ ps
        Player2 -> lsPlayer2 .~ ps

endGameScoring :: LacunaState -> LacunaState
endGameScoring ls
  | gameOver =
      ls
        & addClosestFlowersToPlayers
        & setWinner
  | otherwise = ls
  where
    flowersRemaining = ls ^. lsFlowers . each
    addClosestFlowersToPlayers = appEndo $ foldMap (\f -> Endo $ \s -> addFlowerToPlayer (closestPlayerToFlower f ls) f s) flowersRemaining
    turnsTaken playerL = ls ^. playerL . psPawns . to length
    gameOver = turnsTaken lsPlayer1 + turnsTaken lsPlayer2 == 12

setWinner :: LacunaState -> LacunaState
setWinner ls = ls & lsWinner ?~ winner
  where
    winner
      | playerScore lsPlayer1 ls > playerScore lsPlayer2 ls = Player1
      | otherwise = Player2

closestPlayerToFlower :: V2 Int -> LacunaState -> Lens' LacunaState PlayerState
closestPlayerToFlower flower ls
  -- This should do something different on ties
  | minDistance lsPlayer1 < minDistance lsPlayer2 = lsPlayer1
  | otherwise = lsPlayer2
  where
    findMinDistance = foldMap (Just . Min . qd (fromIntegral <$> flower))
    minDistance p = foldMapOf (p . psPawns) findMinDistance ls

playerScore :: Getter LacunaState PlayerState -> LacunaState -> Int
playerScore playerL = getSum . view (playerL . psCaptured . each . to (bool mempty (Sum 1) . (>= 4) . Set.size))

allPieces :: LacunaState -> Set (V2 Double)
allPieces ls =
  fold
    [ Set.map (fmap fromIntegral) $ foldOf each $ ls ^. lsFlowers,
      ls ^. lsPlayer1 . psPawns,
      ls ^. lsPlayer2 . psPawns
    ]

legalPawn :: LacunaState -> V2 Double -> Bool
legalPawn ls pawn = not $ any (pieceIntersectsPiece (fromIntegral piece_size) pawn) others
  where
    others = Set.delete pawn $ allPieces ls

legalLine :: LacunaState -> (V2 Int, V2 Int) -> Bool
legalLine ls (flower1, flower2) = not $ any (lineIntersectsPoint @Double (fromIntegral piece_size) (fromIntegral <$> flower1, fromIntegral <$> flower2)) others
  where
    others =
      Set.delete (fromIntegral <$> flower1) $
        Set.delete (fromIntegral <$> flower2) $
          allPieces ls

pieceIntersectsPiece :: (Ord a, Num a) => a -> V2 a -> V2 a -> Bool
pieceIntersectsPiece pieceSize a b = qd a b <= ((pieceSize + pieceSize) * (pieceSize + pieceSize))

lineIntersectsPoint :: (Fractional a, Ord a) => a -> (V2 a, V2 a) -> V2 a -> Bool
lineIntersectsPoint pieceSize l o = lineToPointQuadrance l o <= (pieceSize * pieceSize) && inBoundingBox l o

lineToPointQuadrance :: (Fractional a) => (V2 a, V2 a) -> V2 a -> a
lineToPointQuadrance (v1@(V2 x1 y1), v2@(V2 x2 y2)) (V2 x0 y0) = (num * num) / qd v1 v2
  where
    num = abs $ ((x2 - x1) * (y1 - y0)) - ((x1 - x0) * (y2 - y1))

inBoundingBox :: Ord a => (V2 a, V2 a) -> V2 a -> Bool
inBoundingBox (V2 x1 y1, V2 x2 y2) (V2 x0 y0)
  | x1 == x2 = y0 >= minY && y0 <= maxY
  | y1 == y2 = x0 >= minX && x0 <= maxX
  | otherwise = x0 >= minX && x0 <= maxX && y0 >= minY && y0 <= maxY
  where
    maxX = max x1 x2
    maxY = max y1 y2
    minX = min x1 x2
    minY = min y1 y2

closestLine :: LacunaState -> V2 Double -> Maybe (V2 Int, V2 Int)
closestLine ls p = closestCandidate (ls ^.. lsFlowers . each . to toList) <&> \(a, b, _) -> (a, b)
  where
    distanceToEndpoints :: (V2 Int, V2 Int, Double) -> Double
    distanceToEndpoints (a, b, _) = min (qd (fromIntegral <$> a) p) (qd (fromIntegral <$> b) p)
    closestCandidate = listToMaybe . L.sortOn (\x -> (distanceToEndpoints x, view _3 x)) . candidateLines . fLines
    candidateLines :: [(V2 Int, V2 Int)] -> [(V2 Int, V2 Int, Double)]
    candidateLines = mapMaybe $
      \(a, b) -> do
        let d = lineToPointQuadrance (fromIntegral <$> a, fromIntegral <$> b) p
        guard $ inBoundingBox (fromIntegral <$> a, fromIntegral <$> b) p && d <= (15 * 15)
        pure (a, b, d)
    fLines = concatMap $ \f -> [(x, y) | x <- f, y <- f, x /= y]