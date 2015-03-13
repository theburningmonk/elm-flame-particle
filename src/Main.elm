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
ticks = Signal.foldp (\_ n -> n + 1) 0 (Time.fps 20)

type alias Particle = {x:Float, y:Float, radius:Float, color:Color, seed:Float, t:Int}
type alias State = {particles:List Particle, seed:Random.Seed}

defaultState = {particles=[], seed=Random.initialSeed 42}

step : Float -> State -> State
step t state = state |> prune |> update |> spawn t

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
      in alpha > 0 && p.radius > 0)
  in {state | particles<-newParticles}

update : State -> State
update state = 
  let newParticles =
    state.particles |> List.map (\p -> 
      let {red, green, blue, alpha} = toRgb p.color
          newR = red   + (sin (toFloat p.t * p.seed) |> (*) 50 |> floor)
          newG = green + (sin (toFloat p.t * p.seed) |> (*) 25 |> floor)
          newB = blue
          newA = (alpha - 0.014 * p.seed) * 0.97
          newColor = rgba newR newG newB newA
      in {p | x <- p.x - sin (toFloat p.t * 0.386),
              y <- p.y + 1,
              radius <- p.radius - 0.03,
              color  <- newColor,
              t <- p.t + 1})
  in {state | particles<-newParticles}

spawn : Float -> State -> State
spawn t state =
    let newParticles = floor t |> genParticles
        (n, seed) = Random.generate (Random.int 1 5) state.seed
        (randParticles, newSeed) = 
            List.foldl 
                (\_ (acc, seed') -> 
                    let (newP, newSeed) = makeParticle seed'
                    in (newP::acc, newSeed))
                ([], seed) 
                [1..n]
    in {state | particles<-List.concat [newParticles, randParticles, state.particles]
              , seed     <-newSeed}

makeParticle : Random.Seed -> (Particle, Random.Seed)
makeParticle seed =
    let (x, seed1) = Random.generate (Random.float -10 10) seed
        (y, seed2) = Random.generate (Random.float -5 5) seed1
        (r, seed3) = Random.generate (Random.int 188 255) seed2
        (g, seed4) = Random.generate (Random.int 30 80) seed3
        (b, seed5) = Random.generate (Random.int 10 50)  seed4
        (alpha,  seed6) = Random.generate (Random.float 0.2 0.8) seed5
        (radius, seed7) = Random.generate (Random.int 1 5) seed6
        (pSeed,  seed8) = Random.generate (Random.float 0.1 1) seed7

        particle = { x=x, y=y, radius=5,
                     color=rgba r g b alpha,
                     seed=pSeed,
                     t=0 }
    in (particle, seed8)

genParticles : Int -> List Particle
genParticles t =
    [
      {x=-3,  y=2,  radius=6, color=rgba 255 10 10 0.95, seed=0.05, t=6},
      {x=2,   y=2,  radius=5, color=rgba 155 73 10 0.86, seed=0.5, t=4},
      {x=-1,  y=1,  radius=5, color=rgba 255 98 51 0.86, seed=(t |> toFloat |> sin) * 0.9, t=t%10},
      {x=1,   y=0,  radius=5, color=rgba 111 40 40 0.5,  seed=0.03, t=9},
      {x=1,   y=0,  radius=5, color=rgba 170 83 32 0.86, seed=0.4 + (t |> toFloat |> sin) * 0.362, t=t%3},
      {x=0.7, y=1,  radius=5, color=rgba 168 83 11 0.98, seed=0.8, t=t%4},
      {x=1,   y=0,  radius=5, color=rgba 177 72 17 0.85, seed=1.154, t=0},
      {x=-0.5, y=1, radius=5, color=rgba 189 20 17 0.85, seed=0.5, t=3}
    ]