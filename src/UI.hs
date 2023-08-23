{-# LANGUAGE ViewPatterns #-}

module UI (module UI) where

import CanvasPoint
import Control.Lens
import Graphics.Blank
import Import hiding (to, view, (^.), (^..))
import Lacuna
import Linear
import RIO.State

data UITurnState = UITurnStartState | UITurnLineSelected (CanvasPoint Int, CanvasPoint Int)
  deriving (Show)

data UIState = UIState
  { _uisLacunaState :: LacunaState,
    _uisTurnState :: UITurnState,
    _uisMousePos :: CanvasPoint Double
  }
  deriving (Show)

$(makeLenses ''UIState)

initialUIState :: LacunaState -> UIState
initialUIState _uisLacunaState =
  UIState
    { _uisLacunaState,
      _uisTurnState = UITurnStartState,
      _uisMousePos = mkCanvasPointUnsafe $ V2 0 0
    }

data UIEvents = UIEvents
  { uieMousePosition :: CanvasPoint Double,
    uieMouseUp :: Maybe (CanvasPoint Double),
    uieKeyUp :: Maybe ()
  }

type Animation = [Canvas ()]

updateUIState :: MonadState UIState m => UIEvents -> m ()
updateUIState UIEvents {..} = do
  ls <- use uisLacunaState
  uisMousePos .= uieMousePosition
  use uisTurnState >>= \case
    UITurnStartState -> case closestLine ls . fromCanvasPoint =<< uieMouseUp of
      Nothing -> pure ()
      Just l
        | legalLine ls l -> do
            uisTurnState .= UITurnLineSelected (bimap toCanvasPoint toCanvasPoint l)
        | otherwise -> pure ()
    UITurnLineSelected l -> case uieKeyUp of
      Just () -> do
        uisTurnState .= UITurnStartState
      Nothing -> case uieMouseUp of
        Just ((`projectOnLineBounded` l) -> p)
          | legalPawn ls (fromCanvasPoint p) -> do
              uisLacunaState
                %= flip
                  move
                  (PlacePawn (bimap fromCanvasPoint fromCanvasPoint l) (fromCanvasPoint p))
              uisTurnState .= UITurnStartState
          | otherwise -> pure ()
        Nothing -> pure ()

projectOnLineBounded :: CanvasPoint Double -> (CanvasPoint Int, CanvasPoint Int) -> CanvasPoint Double
projectOnLineBounded canvasPoint (fmap fromIntegral -> a, fmap fromIntegral -> b)
  | projection <= minP = minP
  | projection >= maxP = maxP
  | otherwise = projection
  where
    minP = lerpByDistance (fromIntegral (piece_size + piece_size) + 0.01) a b
    maxP = lerpByDistance (fromIntegral (piece_size + piece_size) + 0.01) b a
    projection = project (b - a) (canvasPoint - a) + a

lerpByDistance :: Double -> CanvasPoint Double -> CanvasPoint Double -> CanvasPoint Double
lerpByDistance d a b = d *^ normalize (b - a) + a