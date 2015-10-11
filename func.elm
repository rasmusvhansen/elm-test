import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Keyboard
import Time exposing (..)
import Window

type alias Arrow = {x: Int, y: Int}

type alias Pos = {x: Float, y: Float}


position: Pos
position = 
    { x = 0, y = 0 }

getColor: Pos -> Color
getColor pos =
    let
        g = round pos.x
        b = round pos.y
    in
        rgba 255 g b 0.7 

display: (Int, Int) -> Pos -> Element
display (w, h) pos = 
    collage w h [
     ngon 6 65
        |> filled(getColor(pos))
        |> move(pos.x, pos.y)
        |> rotate (degrees -1 * pos.x)
    ]
    
red : Color
red = 
    rgba 255 0 0 0.5

update: (Float, Arrow) -> Pos -> Pos
update (delta, arrow) pos = 
    { pos |
     x <- pos.x + toFloat(arrow.x) * 10.0 * delta,
     y <- pos.y + toFloat(arrow.y) * 10.0 * delta
    }

input : Signal (Float, Arrow)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

main: Signal Element
main = 
    Signal.map2 display Window.dimensions (Signal.foldp update position input)
 