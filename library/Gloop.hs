module Gloop where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified SDL


play
    :: Text.Text
    -> SDL.WindowConfig
    -> SDL.RendererConfig
    -> world
    -> Duration
    -> Handle world
    -> Step world
    -> Render world
    -> IO ()
play title windowConfig rendererConfig world duration handle step render = do
    SDL.initializeAll

    window <- SDL.createWindow title windowConfig
    let driver = -1
    renderer <- SDL.createRenderer window driver rendererConfig

    epoch <- SDL.ticks
    let delta = 0
    let state = State epoch delta world

    loop window renderer state duration handle step render


type Duration = Word.Word32


type Handle world = world -> SDL.Event -> IO world


type Step world = world -> IO world


type Render world = world -> SDL.Renderer -> Smear -> IO ()


type Smear = Double


data State world = State
    { stateTime :: Time
    , stateDelta :: Delta
    , stateWorld :: world
    } deriving (Eq, Show)


type Time = Word.Word32


type Delta = Word.Word32


loop
    :: SDL.Window
    -> SDL.Renderer
    -> State world
    -> Duration
    -> Handle world
    -> Step world
    -> Render world
    -> IO ()
loop window renderer state duration handle step render = do
    events <- SDL.pollEvents
    world <- Monad.foldM handle (stateWorld state) events

    now <- SDL.ticks
    let elapsed = now - stateTime state
    let delta = stateDelta state + elapsed
    (newWorld, newDelta) <- simulate world duration delta step

    let smear = fromIntegral newDelta / fromIntegral duration
    SDL.rendererDrawColor renderer SDL.$= 0
    SDL.clear renderer
    render newWorld renderer smear
    SDL.present renderer

    let newState = State now newDelta newWorld
    loop window renderer newState duration handle step render


simulate
    :: world
    -> Duration
    -> Delta
    -> Step world
    -> IO (world, Delta)
simulate world duration delta step = do
    if delta < duration
        then do
            pure (world, delta)
        else do
            newWorld <- step world
            let newDelta = delta - duration
            simulate newWorld duration newDelta step
