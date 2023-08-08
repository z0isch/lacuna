{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Birdson (birdson, debugImage, initialBirdson)
import Codec.Picture (writePng)
import Import
import RIO.State (execStateT)

run :: RIO App ()
run = do
  b <- execStateT birdson initialBirdson
  logInfo $ displayShow b
  liftIO $ writePng "tmp.png" $ debugImage b
