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

    let textTitle = StringConv.toS title
    window <- SDL.createWindow textTitle windowConfig

    now <- SDL.ticks
    let state = State world now 0

    loop window duration state render handle step


-- | The game state, which tracks the current time, lag, and world.
data State world = State
    { stateWorld :: world
    -- ^ The world.
    , stateTick :: Word.Word32
    -- ^ The current time in milliseconds.
    , stateLag :: Word.Word32
    -- ^ The current lag, which is the time in milliseconds since the last
    -- simulation step.
    }


-- | The main game loop. Handles polling for events, applying those events to
-- the world, running the simulation, and rendering the world.
loop
    :: SDL.Window
    -- ^ The window to render the world into.
    -> Word.Word32
    -- ^ The duration in milliseconds of each step of the world.
    -> State world
    -- ^ The current state of the world.
    -> (SDL.Window -> Double -> world -> IO ())
    -- ^ A function for rendering the world. The second argument is a smear
    -- value in the interval [0, 1).
    -> (SDL.Event -> world -> Maybe world)
    -- ^ A function for handling input events. Return 'Nothing' to stop the
    -- game loop.
    -> (world -> world)
    -- ^ A function that steps the world a single iteration.
    -> IO ()
loop window duration state render handle step = do
    let eldnah world event = handle event world
    let initialWorld = stateWorld state
    events <- SDL.pollEvents
    let maybeWorld = Monad.foldM eldnah initialWorld events
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


-- | Steps the world a single iteration if enough time has passed. Otherwise
-- returns the same world.
simulate
    :: Word.Word32
    -- ^ The duration in milliseconds of each step of the world.
    -> (world -> world)
    -- ^ A function that steps the world a single iteration.
    -> world
    -- ^ The current state of the world.
    -> Word.Word32
    -- ^ The current lag, which is the time in milliseconds since the last
    -- simulation step.
    -> (world, Word.Word32)
    -- ^ The new world and the new lag.
simulate duration step world lag = do
    let newWorld = step world
    let newLag = lag - duration
    case compare lag duration of
        LT -> (world, lag)
        _ -> simulate duration step newWorld newLag
