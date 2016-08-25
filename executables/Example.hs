import SDL (($=))

import qualified Data.Text as Text
import qualified Gloop
import qualified Linear as Linear
import qualified Linear.Affine as Linear
import qualified SDL
import qualified System.Exit as Exit


main :: IO ()
main = do
    Gloop.play
        -- Window title.
        (Text.pack "Gloop example")
        -- Window config.
        SDL.defaultWindow
            { SDL.windowResizable = True
            }
        -- Renderer config.
        SDL.defaultRenderer
            { SDL.rendererType = SDL.AcceleratedRenderer
            }
        -- Initial world. We're just going to use an integer that tracks the
        -- number of steps we've simulated.
        0
        -- Duration of a simulation step in milliseconds. The simulation is
        -- decoupled from the rendering and each step is the exact same amount
        -- of time. For reference, 60 Hz is 16.6... milliseconds.
        100
        -- Event handler. You always need to handle quit and close events,
        -- otherwise there's no way to leave your application. This handler
        -- prints the events so you can see what's happening.
        (\ world event -> do
            print event
            case SDL.eventPayload event of
                SDL.QuitEvent -> Exit.exitSuccess
                SDL.WindowClosedEvent _ -> Exit.exitSuccess
                _ -> pure world)
        -- Simulate one step of the world.
        (\ world -> do
            let newWorld = world + 1
            pure newWorld)
        -- Renders the world. The "smear" argument is a floating point number
        -- (0.0 <= smear < 1.0). It tells you how close to the next simulation
        -- step you are. This renderer renders a green box that constantly gets
        -- bigger and a red box that corresponds to the smear.
        (\ world renderer smear -> do
            SDL.rendererDrawColor renderer $= Linear.V4 0 255 0 0
            SDL.drawRect renderer (Just (SDL.Rectangle Linear.origin world))

            SDL.rendererDrawColor renderer $= Linear.V4 255 0 0 0
            let size = fromIntegral (ceiling (smear * 100) :: Word)
            SDL.drawRect renderer (Just (SDL.Rectangle Linear.origin size)))
