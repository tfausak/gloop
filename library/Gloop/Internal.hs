{-# LANGUAGE FlexibleContexts #-}

module Gloop.Internal where

import qualified Control.Monad as Monad
import qualified Data.String.Conv as StringConv
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified SDL


-- | Plays a game in a window. Uses the game loop with a fixed update time step
-- and variable rendering described by Robert Nystrom's
-- <http://gameprogrammingpatterns.com Game Programming Patterns>.
--
-- > import qualified Gloop
-- >
-- > data World = World { step :: Int }
-- >
-- > main :: IO ()
-- > main = Gloop.play
-- >     -- title
-- >     "Example"
-- >     -- window config
-- >     Gloop.defaultWindow { Gloop.windowResizable = True }
-- >     -- step duration (ms)
-- >     100
-- >     -- initial world
-- >     World { step = 0 }
-- >     -- renders the world
-- >     (\ _window _smear _world -> do
-- >         pure ())
-- >     -- handles events
-- >     (\ event world -> case Gloop.eventPayload event of
-- >         Gloop.WindowClosedEvent _ -> Nothing
-- >         _ -> Just world)
-- >     -- steps the world
-- >     (\ world -> world { step = step world + 1 })
play
    :: (StringConv.StringConv title Text.Text)
    => title
    -- ^ The title to use for the game's window.
    -> SDL.WindowConfig
    -- ^ The configuration options for the game's window. Usually
    -- 'SDL.defaultWindow'.
    -> Word.Word32
    -- ^ The duration in milliseconds of each step of the world.
    -> world
    -- ^ The initial world to start with.
    -> (SDL.Window -> Double -> world -> IO ())
    -- ^ A function for rendering the world. The second argument is a smear
    -- value in the interval [0, 1).
    -> (SDL.Event -> world -> Maybe world)
    -- ^ A function for handling input events. Return 'Nothing' to stop the
    -- game loop.
    -> (world -> world)
    -- ^ A function that steps the world a single iteration.
    -> IO ()
play title windowConfig duration world render handle step = do
    SDL.initializeAll

    window <- SDL.createWindow (StringConv.toS title) windowConfig

    now <- SDL.ticks
    let state = State world now 0

    loop window duration state render handle step


data State world = State
    { stateWorld :: world
    , stateTick :: Word.Word32
    , stateLag :: Word.Word32
    }


loop
    :: SDL.Window
    -> Word.Word32
    -> State world
    -> (SDL.Window -> Double -> world -> IO ())
    -> (SDL.Event -> world -> Maybe world)
    -> (world -> world)
    -> IO ()
loop window duration state render handle step = do
    events <- SDL.pollEvents
    let maybeWorld = Monad.foldM (flip handle) (stateWorld state) events
    case maybeWorld of
        Nothing -> pure ()
        Just world -> do
            now <- SDL.ticks
            let delta = now - stateTick state
            let lag = stateLag state + delta
            let (newWorld, newLag) = simulate duration step world lag

            let smear = fromIntegral lag / fromIntegral duration
            render window smear newWorld

            let newState = State newWorld now newLag

            loop window duration newState render handle step


simulate
    :: Word.Word32
    -> (world -> world)
    -> world
    -> Word.Word32
    -> (world, Word.Word32)
simulate duration step world lag =
    if lag < duration
    then (world, lag)
    else do
        let newWorld = step world
        let newLag = lag - duration
        simulate duration step newWorld newLag
