import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Signal (..)
import Signal
import Random
import List
import Text
import Time
import Window
import Debug

ticks : Signal Float
ticks = Signal.foldp (\_ n -> n + 1) 0 (Time.fps 10)

type alias Particle = {x:Float, y:Float, radius:Float, color:Color, seed:Float}
type alias State = {particles:List Particle, seed:Random.Seed}

defaultState = {particles=[], seed=Random.initialSeed 42}

step : Float -> State -> State
step t state = state |> prune |> update t |> spawn 15

state : Signal State
state = Signal.foldp step defaultState ticks

display : (Int,Int) -> State -> Element
display (w, h) state =
  let background = rect (toFloat w) (toFloat h) |> filled black
      content =
        state.particles 
        |> Debug.watch "Particles"
        |> List.map (\p ->
          circle p.radius
          |> filled p.color
          |> move (p.x, p.y))
  in collage w h (background::content)

main : Signal Element
main = display<~Window.dimensions~state

prune : State -> State
prune state = 
  let newParticles = 
    state.particles |> List.filter (\p -> 
      let {red, green, blue, alpha} = toRgb p.color
      in alpha > 0)
  in {state | particles<-newParticles}

update : Float -> State -> State
update t state = 
  let newParticles =
    state.particles |> List.map (\p -> 
      let {red, green, blue, alpha} = toRgb p.color
          newR = red   + (sin (t * p.seed) |> (*) 30 |> floor)
          newG = green + (sin (t * p.seed) |> (*) 15 |> floor)
          newB = blue --  + (sin (t * p.seed) |> (*) 15 |> floor)
          newA = (alpha - 0.014 * p.seed) * 0.98
          newColor = rgba newR newG newB newA
      in {p | x <- p.x - sin (t * 0.386) * p.seed,
              y <- p.y + 1.5,
              radius <- p.radius - 0.03,
              color  <- newColor})
  in {state | particles<-newParticles}

spawn : Int -> State -> State
spawn n state =
    case n of
        0 -> state
        n -> 
          let (newParticle, newSeed) = makeParticle state.seed
          in spawn (n-1) {state | particles<-newParticle::state.particles
                                , seed     <-newSeed}

makeParticle : Random.Seed -> (Particle, Random.Seed)
makeParticle seed =
    let (x, seed1) = Random.generate (Random.float -20 20) seed
        (y, seed2) = Random.generate (Random.float -10 10) seed1
        (r, seed3) = Random.generate (Random.int 200 255)  seed2
        (g, seed4) = Random.generate (Random.int 30 100)   seed3
        (b, seed5) = Random.generate (Random.int 10 70)    seed4
        (alpha,  seed6) = Random.generate (Random.float 0 1) seed5
        (radius, seed7) = Random.generate (Random.int 4 8)   seed6
        (pSeed,  seed8) = Random.generate (Random.float 0 1) seed7

        particle = { x=x, y=y, radius=5,
                     color=rgba r g b alpha,
                     seed=pSeed }
    in (particle, seed8)