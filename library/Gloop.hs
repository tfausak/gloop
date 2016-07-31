module Gloop
    ( play
    ) where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified SDL


-- | Plays a game in a window. Uses the game loop with a fixed update time step
-- and variable rendering described by Robert Nystrom's
-- <http://gameprogrammingpatterns.com Game Programming Patterns>.
--
-- > import qualified Data.Text as Text
-- > import qualified Gloop
-- > import qualified SDL
-- >
-- > data World = World { step :: Int }
-- >
-- > main :: IO ()
-- > main = Gloop.play
-- >     -- title
-- >     (Text.pack "Example")
-- >     -- window config
-- >     SDL.defaultWindow { SDL.windowResizable = True }
-- >     -- renderer config
-- >     SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
-- >     -- step duration (ms)
-- >     100
-- >     -- initial world
-- >     World { step = 0 }
-- >     -- renders the world
-- >     (\ _renderer _extrapolation _world -> do
-- >         pure ())
-- >     -- handles events
-- >     (\ event world -> case SDL.eventPayload event of
-- >         SDL.WindowClosedEvent _ -> Nothing
-- >         _ -> Just world)
-- >     -- steps the world
-- >     (\ world -> world { step = step world + 1 })
play
    :: Text.Text
    -- ^ The title to use for the game's window.
    -> SDL.WindowConfig
    -- ^ The configuration options for the game's window. Usually
    -- 'SDL.defaultWindow'.
    -> SDL.RendererConfig
    -- ^ The configuration options for the game's renderer. Usually
    -- 'SDL.defaultRenderer'.
    -> Word.Word32
    -- ^ The duration in milliseconds of each step of the world.
    -> world
    -- ^ The initial world to start with.
    -> (SDL.Renderer -> Double -> world -> IO ())
    -- ^ A function for rendering the world. The second argument is an
    -- extrapolation value in the interval [0, 1).
    -> (SDL.Event -> world -> Maybe world)
    -- ^ A function for handling input events. Return 'Nothing' to stop the
    -- game loop.
    -> (world -> world)
    -- ^ A function that steps the world a single iteration.
    -> IO ()
play title windowConfig rendererConfig duration world render handle step = do
    SDL.initializeAll

    window <- SDL.createWindow title windowConfig

    let driver = -1 -- First available rendering driver that supports config.
    renderer <- SDL.createRenderer window driver rendererConfig

    now <- SDL.ticks
    let state = State world now 0

    loop renderer duration state render handle step


data State world = State
    { stateWorld :: world
    , stateTick :: Word.Word32
    , stateLag :: Word.Word32
    }


loop
    :: SDL.Renderer
    -> Word.Word32
    -> State world
    -> (SDL.Renderer -> Double -> world -> IO ())
    -> (SDL.Event -> world -> Maybe world)
    -> (world -> world)
    -> IO ()
loop renderer duration state render handle step = do
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
            SDL.clear renderer
            render renderer smear newWorld
            SDL.present renderer

            let newState = State newWorld now newLag

            loop renderer duration newState render handle step


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
