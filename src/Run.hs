{-# LANGUAGE ViewPatterns #-}

module Run (run) where

import CanvasPoint
import Control.Concurrent (forkIO)
import Graphics.Blank
import Import
import Lacuna
import Linear (V2 (..))
import RIO.NonEmpty qualified as NE
import RIO.State (get, gets, runStateT)
import Render
import UI

run :: RIO App ()
run = do
  let options =
        3000
          { events =
              [ "keypress",
                "keydown",
                "keyup",
                "mousemove",
                "mouseup"
              ]
          }
  liftIO $ blankCanvas options $ \context -> do
    mouseUpV <- newEmptyTMVarIO
    mousePosV <- newTVarIO (V2 0 0)
    keyUpV <- newEmptyTMVarIO
    void $ forkIO $ forever $ do
      e <- atomically $ readTChan (eventQueue context)
      atomically $ case eType e of
        "mousemove" -> case ePageXY e of
          Nothing -> error "We should have a position"
          Just (x, y) -> writeTVar mousePosV $ V2 x y
        "mouseup" -> case ePageXY e of
          Nothing -> error "We should have a position"
          Just (x, y) -> void $ tryPutTMVar mouseUpV $ V2 x y
        "keyup" -> void $ tryPutTMVar keyUpV ()
        _ -> pure ()

    ls <- initialLacunaState

    let getUIEvents = atomically $ do
          uieMousePosition <- mkCanvasPointUnsafe <$> readTVar mousePosV
          uieMouseUp <- fmap mkCanvasPointUnsafe <$> tryTakeTMVar mouseUpV
          uieKeyUp <- tryTakeTMVar keyUpV
          pure UIEvents {..}

    void $ flip runStateT (initialUIState ls) $ forever $ do
      uiEvents <- getUIEvents
      prevUIState <- get
      updateUIState uiEvents
      (animation, renderedState) <- (NE.init &&& NE.last) <$> gets (renderUIState prevUIState)
      renderAnimation context animation
      liftIO $ send context $ clearCanvas >> renderedState
