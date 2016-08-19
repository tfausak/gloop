# [Gloop][]

Gloop provides SDL2 game loops.

Gloop is a thin wrapper around [the sdl2 package][]. It provides a single
function called `play` that handles a game loop with a fixed update time step
and variable rendering as described by Robert Nystrom's [Game Programming
Patterns][]. Its interface is modeled after [the gloss package][].

``` haskell
import qualified Gloop

data World = World { step :: Int }

main :: IO ()
main = Gloop.play
    -- title
    "Example"
    -- window config
    Gloop.defaultWindow { Gloop.windowResizable = True }
    -- step duration (ms)
    100
    -- initial world
    World { step = 0 }
    -- renders the world
    (\ _window _extrapolation _world -> do
        pure ())
    -- handles events
    (\ event world -> case Gloop.eventPayload event of
        Gloop.WindowClosedEvent _ -> Nothing
        _ -> Just world)
    -- steps the world
    (\ world -> world { step = step world + 1 })
```

[Gloop]: https://github.com/tfausak/gloop
[the sdl2 package]: https://www.stackage.org/package/sdl2
[Game Programming Patterns]: http://gameprogrammingpatterns.com
[the gloss package]: https://hackage.haskell.org/package/gloss
