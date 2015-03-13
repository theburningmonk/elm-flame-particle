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
ticks = Signal.foldp (\_ n -> n + 1) 0 (Time.fps 8)

type alias Particle = {x:Float, y:Float, radius:Float, color:Color, seed:Float}
type alias State = List Particle

defaultState = []

step : Float -> State -> State
step t state = state |> prune |> update t |> generate t 15

state : Signal State
state = Signal.foldp step defaultState ticks

display : (Int,Int) -> State -> Element
display (w, h) state =
  let background = rect (toFloat w) (toFloat h) |> filled black
      content =
        state 
        |> Debug.watch "State"
        |> List.map (\p ->
          circle p.radius
          |> filled p.color
          |> move (p.x, p.y))
  in collage w h (background::content)

main : Signal Element
main = display<~Window.dimensions~state

prune : State -> State
prune state = 
  state |> List.filter (\p -> 
    let {red, green, blue, alpha} = toRgb p.color
    in alpha > 0)

update : Float -> State -> State
update t state = 
  state |> List.map (\p -> 
    let {red, green, blue, alpha} = toRgb p.color
        newR = red   + (sin (t * p.seed) |> (*) 40 |> floor)
        newG = green + (sin (t * p.seed) |> (*) 33 |> floor)
        newB = blue --  + (sin (t * p.seed) |> (*) 15 |> floor)
        newA = (alpha - 0.016 * p.seed) * 0.97
        newColor = rgba newR newG newB newA
    in {p | x <- p.x - sin (t * 0.436),
            y <- p.y + 1,
            radius <- p.radius - 0.03,
            color  <- newColor})

generate : Float -> Int -> State -> State
generate t n state =
    case n of
        0 -> state
        n -> generate t (n-1) (makeParticle (floor t + n)::state)

makeParticle : Int -> Particle
makeParticle n =
    let seed = Random.initialSeed n
        ([x, y], seed')   = Random.generate (Random.list 2 (Random.float -10 10)) seed
        ([r], seed'')     = Random.generate (Random.list 1 (Random.int 175 255)) seed'
        ([g, b], seed''') = Random.generate (Random.list 2 (Random.int 1 100)) seed''
        ([alpha, pSeed], _) = Random.generate (Random.list 2 (Random.float 0 1)) seed'''
    in { x = x,
         y = y,
         radius = 5,
         color  = rgba r g b alpha,
         seed   = pSeed
       }