{-# LANGUAGE ViewPatterns #-}

module Render (module Render) where

import CanvasPoint
import Control.Comonad (extend)
import Control.Lens
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Zipper (Zipper)
import Data.List.NonEmpty.Zipper qualified as Z
import Graphics.Blank
import Import hiding (Lens', to, (^.), (^..))
import Lacuna
import Linear
import RIO.Map qualified as Map
import RIO.Set qualified as Set
import UI

player1Color :: Text
player1Color = "#C0BFBF"

player2Color :: Text
player2Color = "#D79150"

flowerColors :: (Text, Text, Text, Text, Text, Text, Text)
flowerColors = ("#FF5B36", "#00998A", "#907531", "#FF8C2A", "#F3C2B2", "#6C5291", "#004389")

type FlowerLookup = CanvasPoint Int -> Text

getFlowerColor :: LacunaState -> CanvasPoint Int -> Text
getFlowerColor ls = fromMaybe "red" . (`Map.lookup` allFlowers)
  where
    allFlowers =
      foldMap
        flowersWithColors
        [ ls ^. lsFlowers,
          ls ^. lsPlayer1 . psCaptured,
          ls ^. lsPlayer2 . psCaptured
        ]
    flowersWithColors fs = fold $ zipWith (\ps c -> Map.fromList $ (,c) <$> ps) (fs ^.. each . to (fmap toCanvasPoint . toList)) (flowerColors ^.. each)

renderUIState :: UIState -> UIState -> NonEmpty (Canvas ())
renderUIState prevUIState uiState = do
  let flowersList = toCanvasPoint <$> uiState ^. uisLacunaState . lsFlowers . each . to toList
      flowerLookup = getFlowerColor (uiState ^. uisLacunaState)
  NE.prependList (flowerMoves prevUIState uiState) $ case uiState ^. uisTurnState of
    UITurnStartState ->
      pure $ do
        drawBackground
        drawPlayerState flowerLookup player1StateStartP (uiState ^. uisLacunaState . lsPlayer1 . psCaptured)
        drawPlayerState flowerLookup player2StateStartP (uiState ^. uisLacunaState . lsPlayer2 . psCaptured)
        drawClosestLine (uiState ^. uisLacunaState) (uiState ^. uisMousePos)
        traverse_ (\f -> drawFlower flowerLookup f (fromIntegral <$> f)) flowersList
        traverse_ (drawPawn (uiState ^. uisLacunaState) . (,player1Color) . toCanvasPoint) (uiState ^. uisLacunaState . lsPlayer1 . psPawns)
        traverse_ (drawPawn (uiState ^. uisLacunaState) . (,player2Color) . toCanvasPoint) (uiState ^. uisLacunaState . lsPlayer2 . psPawns)

        case uiState ^. uisLacunaState . lsWinner of
          Just winner -> do
            fillStyle $ case winner of
              Player1 -> player1Color
              Player2 -> player2Color
            font "48px serif"
            let xOffset = case winner of
                  Player1 -> 25
                  Player2 -> 20 + 800 + 400 + 25 + 20
            fillText (tshow winner <> " wins!", xOffset, 250)
          Nothing -> pure ()
    UITurnLineSelected (a, b) ->
      pure $ do
        drawBackground
        drawPlayerState flowerLookup player1StateStartP (uiState ^. uisLacunaState . lsPlayer1 . psCaptured)
        drawPlayerState flowerLookup player2StateStartP (uiState ^. uisLacunaState . lsPlayer2 . psCaptured)
        beginPath ()
        lineWidth 2
        strokeStyle $ getFlowerColor (uiState ^. uisLacunaState) a
        moveTo $ CanvasPoint.toTuple $ fromIntegral <$> a
        lineTo $ CanvasPoint.toTuple $ fromIntegral <$> b
        stroke ()
        traverse_ (\f -> drawFlower flowerLookup f (fromIntegral <$> f)) flowersList
        traverse_ (drawPawn (uiState ^. uisLacunaState) . (,player1Color) . toCanvasPoint) (uiState ^. uisLacunaState . lsPlayer1 . psPawns)
        traverse_ (drawPawn (uiState ^. uisLacunaState) . (,player2Color) . toCanvasPoint) (uiState ^. uisLacunaState . lsPlayer2 . psPawns)
        drawPawn (uiState ^. uisLacunaState) (projectOnLineBounded (uiState ^. uisMousePos) (a, b), uiState ^. uisLacunaState . currentPlayerColor)

-- TODO: Still not right
flowerMoves :: UIState -> UIState -> [Canvas ()]
flowerMoves prevUIState uiState = maybe [] (concat . toList . extend drawZipperFlower) changedFlowersZipper
  where
    flowerLookup = getFlowerColor (uiState ^. uisLacunaState)
    flowers = uisLacunaState . lsFlowers . each
    changedFlowers = foldMap (uncurry Set.difference) $ zip (prevUIState ^.. flowers) (uiState ^.. flowers)
    allPlayerFlowers =
      Map.fromList $
        playerStateFlowers player1StateStartP (uiState ^. uisLacunaState . lsPlayer1 . psCaptured)
          <> playerStateFlowers player2StateStartP (uiState ^. uisLacunaState . lsPlayer2 . psCaptured)
    changedFlowersZipper = Z.fromNonEmpty <$> nonEmpty (Map.toList $ Map.restrictKeys allPlayerFlowers changedFlowers)
    drawZipperFlower :: Zipper (V2 Int, CanvasPoint Double) -> [Canvas ()]
    drawZipperFlower flowerZ =
      let (flower, p) = Z.current flowerZ
          (Set.fromList -> alreadyMovedFlowers) = fst <$> Z.lefts flowerZ
          notMovedFlowers = Set.insert flower $ Set.fromList $ fst <$> Z.rights flowerZ
          removeFlowers (a, b, c, d, e, f, g) =
            ( a `Set.difference` notMovedFlowers,
              b `Set.difference` notMovedFlowers,
              c `Set.difference` notMovedFlowers,
              d `Set.difference` notMovedFlowers,
              e `Set.difference` notMovedFlowers,
              f `Set.difference` notMovedFlowers,
              g `Set.difference` notMovedFlowers
            )
          addFlowers (a, b, c, d, e, f, g) =
            ( a `Set.difference` alreadyMovedFlowers,
              b `Set.difference` alreadyMovedFlowers,
              c `Set.difference` alreadyMovedFlowers,
              d `Set.difference` alreadyMovedFlowers,
              e `Set.difference` alreadyMovedFlowers,
              f `Set.difference` alreadyMovedFlowers,
              g `Set.difference` alreadyMovedFlowers
            )
          stateFlowers = toCanvasPoint <$> prevUIState ^. uisLacunaState . lsFlowers . to addFlowers . each . to toList
       in fmap (/ 150) [1 .. 150] <&> \t -> do
            drawBackground
            drawPlayerState flowerLookup player1StateStartP (uiState ^. uisLacunaState . lsPlayer1 . psCaptured . to removeFlowers)
            drawPlayerState flowerLookup player2StateStartP (uiState ^. uisLacunaState . lsPlayer2 . psCaptured . to removeFlowers)
            traverse_
              ( \f ->
                  if f == toCanvasPoint flower
                    then do
                      drawFlowerOutline $ lerp t p (fromIntegral <$> f)
                      drawFlower' 18 flowerLookup f $ lerp t p (fromIntegral <$> f)
                    else drawFlower flowerLookup f $ fromIntegral <$> f
              )
              stateFlowers
            traverse_ (drawPawn (uiState ^. uisLacunaState) . (,player1Color) . toCanvasPoint) (uiState ^. uisLacunaState . lsPlayer1 . psPawns)
            traverse_ (drawPawn (uiState ^. uisLacunaState) . (,player2Color) . toCanvasPoint) (uiState ^. uisLacunaState . lsPlayer2 . psPawns)

player1StateStartP :: CanvasPoint Double
player1StateStartP = mkCanvasPointUnsafe $ V2 20 20

player2StateStartP :: CanvasPoint Double
player2StateStartP = mkCanvasPointUnsafe $ V2 (20 + 800 + 400 + 25 + 20) 20

drawBackground :: Canvas ()
drawBackground = do
  (w, h) <- (width &&& height) <$> myCanvasContext
  fillStyle "black"
  fillRect (0, 0, w, h)
  beginPath ()
  strokeStyle "#1A134A"
  lineWidth 2
  arc (800, 450, 400 + 15, 0, 2 * pi, False)
  arc (800, 450, 400 + 20, 0, 2 * pi, False)
  arc (800, 450, 400 + 25, 0, 2 * pi, False)
  stroke ()

drawPlayerState :: FlowerLookup -> CanvasPoint Double -> Flowers -> Canvas ()
drawPlayerState flowerColor startP flowers = for_ (playerStateFlowers startP flowers) $ \(flower, p) -> drawFlower flowerColor (toCanvasPoint flower) p

playerStateFlowers :: CanvasPoint Double -> Flowers -> [(V2 Int, CanvasPoint Double)]
playerStateFlowers startP captured =
  concat $
    spacedOutByY (captured ^.. each . to toList) startP
      <&> uncurry spacedOutByX
  where
    spacedOutByX = spacedOut $ \x -> mkCanvasPointUnsafe $ V2 (x * 25) 0
    spacedOutByY = spacedOut $ \x -> mkCanvasPointUnsafe $ V2 0 (x * 25)
    spacedOut f xs p = zip xs $ fmap (\x -> p + f x) [1 ..]

drawFlower :: FlowerLookup -> CanvasPoint Int -> CanvasPoint Double -> Canvas ()
drawFlower = drawFlower' 10

drawFlower' :: Double -> FlowerLookup -> CanvasPoint Int -> CanvasPoint Double -> Canvas ()
drawFlower' s flowerColor flower p = do
  let (x, y) = CanvasPoint.toTuple p
  beginPath ()
  fillStyle $ flowerColor flower
  arc (x, y, s, 0, 2 * pi, False)
  fill ()

drawFlowerOutline :: CanvasPoint Double -> Canvas ()
drawFlowerOutline p = do
  let (x, y) = CanvasPoint.toTuple p
  beginPath ()
  fillStyle "white"
  arc (x, y, 20, 0, 2 * pi, False)
  fill ()

drawPawn :: LacunaState -> (CanvasPoint Double, Text) -> Canvas ()
drawPawn ls (p, c) = do
  let (x, y) = CanvasPoint.toTuple p
  beginPath ()
  fillStyle $ if legalPawn ls (fromCanvasPoint p) then c else "red"
  arc (x, y, 10, 0, 2 * pi, False)
  fill ()
  beginPath ()
  fillStyle "black"
  arc (x, y, 7, 0, 2 * pi, False)
  fill ()

drawClosestLine :: LacunaState -> CanvasPoint Double -> Canvas ()
drawClosestLine ls p = case closestLine ls (fromCanvasPoint p) of
  Nothing -> pure ()
  Just (a, b) -> do
    beginPath ()
    lineWidth $ if legalLine ls (a, b) then 2 else 7
    strokeStyle $ if legalLine ls (a, b) then getFlowerColor ls (toCanvasPoint a) else "red"
    moveTo $ CanvasPoint.toTuple $ fromIntegral <$> toCanvasPoint a
    lineTo $ CanvasPoint.toTuple $ fromIntegral <$> toCanvasPoint b
    stroke ()

currentPlayerColor :: Getter LacunaState Text
currentPlayerColor =
  lsTurn
    . to
      ( \case
          Player1 -> player1Color
          Player2 -> player2Color
      )

renderAnimation :: MonadIO m => DeviceContext -> Animation -> m ()
renderAnimation context animation = for_ animation $ \a -> do
  void $ liftIO $ send context $ clearCanvas >> a
  threadDelay 100

-- animateFlowers :: [([CanvasPoint Int], Text)] -> Animation
-- animateFlowers fs = go <$> fmap (/ 100) [1 .. 100]
--   where
--     interpStep :: Double -> (CanvasPoint Int, a) -> (CanvasPoint Double, a)
--     interpStep t (x, c) = (lerp t (fromIntegral <$> x) (mkCanvasPointUnsafe $ V2 800 450), c)
--     go :: Double -> Canvas ()
--     go t = for_ (concatMap (\(xs, c) -> fmap (,c) xs) fs) $
--       \f ->
--         drawFlower $ interpStep t f