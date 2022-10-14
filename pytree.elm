import Dict 
import Round

navy = rgb 0 0 200

myShapes model =
  [ background
  ,sierpinski (state2num model.state - 1) (state2num model.state) (hsl (degrees 60*model.time) 1 0.2)|> move(-90,-25) |> scale 0.5
  , case model.state of
      Dragging x -> circle 5 |> filled navy |> move (toPos x, -58)
      Waiting x -> circle 4 |> filled navy |> move (toPos x, -58)
      Animating x -> circle 4 |> outlined (solid 0.5) navy |> move (toPos x, -58)
  , List.map ( \ idx -> String.fromInt idx |> text |> centered |> size 4 |> filled (rgb 100 100 200) |> move (toPos (toFloat idx),-59.5) )
      (List.range 0 8)
      |> group
  , roundedRect 90 10 5 |> filled (rgba 255 0 0 0.5)
      |> move (-10,-58)
      |> ( case model.state of 
             Waiting _  -> notifyMouseDownAt StartDragAt

             Dragging _ -> notifyMouseMoveAt MoveDragAt
                             >> notifyMouseUp StopDrag
                             >> notifyLeave StopDrag
             Animating _ -> notifyTap StartAnimating --identity 
                            >> notifyMouseUp StopAnimating
                            >> notifyLeave StopAnimating
         )
  , text (Debug.toString model.state) |> size 3 |> centered |> filled black |> move (0,60)
  ,triangle 5 |> filled darkGreen |> move(40,-58) |> notifyTap StartAnimating
  , square 8 |> filled red |> move(51,-58) |> notifyTap StopAnimating
  
  ]
background = group[square 200
                     |> filled (rgb 255 228 225)
                    ,text "Pythagoras Tree Simulator"
                        |> size 6
                        |> underline
                        |> centered
                        |> filled (rgb 100 0 100)
                        |> move (0,50)
                     ,text "You can use the slider/button to explore the recursive calls."
                        |> size 6
                        |> centered
                        |> filled black
                        |> move (0,-47)
                     , rect 150 10
                        |> outlined (solid 3) yellow
                        |> move (0,-45)
                     , rect 150 10
                        |> outlined (solid 1) (rgb 230 125 50)
                        |> move (0,-45)
                     , rect 80 70
                        |> outlined (solid 3) white
                        |> move (45,5)
                     , rect 80 70
                        |> outlined (solid 1) black
                        |> move (45,5)
                     ,text "It starts with a square."
                        |> size 5
                        |> centered
                        |> sansserif
                        |> filled black
                        |> move (45,32)
                     ,text "Upon this two squares are "
                        |> size 5
                        |> centered
                        |> sansserif
                        |> filled black
                        |> move (45,25)
                      ,text "constructed with area scaled down "
                        |> size 5
                        |> centered
                        |> sansserif
                        |> filled black
                        |> move (45.5,18)
                      ,text "by √2/2 = 0.7 (approx.)"
                        |> size 5
                        |> centered
                        |> sansserif
                        |> filled black
                        |> move (45,11)
                      ,text "such that corners of squares "
                        |> size 5
                        |> centered
                        |> sansserif
                        |> filled black
                        |> move (45,4)
                      ,text "coincide pairwise."
                        |> size 5
                        |> centered
                        |> sansserif
                        |> filled black
                        |> move (45,-3)
                       ,text "This repeats in every step"
                        |> size 5
                        |> bold
                        |> centered
                        |> sansserif
                        |> filled black
                        |> move (45,-10)
                        ,text "resulting in "
                        |> size 5
                        |> bold
                        |> centered
                        |> sansserif
                        |> filled black
                        |> move (45,-17)
                        ,text "PYTHAGORAS TREE !"
                        |> size 5
                        |> bold
                        |> centered
                        |> sansserif
                        |> filled black
                        |> move (45,-24)
                       ]
type alias Point = (Float,Float)

type Msg 
  = Tick Float GetKeyState
  | StartDragAt Point
  | MoveDragAt Point
  | StopDrag
  | StartAnimating
  | StopAnimating
  
  
type alias Model = { time : Float }

type State 
  = Waiting Float
  | Dragging Float
  | Animating Float
  
state2num state =
  case state of
    Waiting x -> x
    Dragging x -> x
    Animating x -> x
    
update msg model 
  = case msg of
      Tick t _   -> { model | time = t
                            , state = case model.state of 
                                Animating old -> 
                                  let new = old + t - model.time
                                  in if new > 8 then Waiting 8 else Animating new 
                                otherwise -> otherwise
                            }
      StartDragAt (x,_) -> 
        case model.state of 
          Waiting _ -> { model | state = Dragging (toGen x) } -- set up for 10 generations
          _ -> model
      MoveDragAt (x,_) -> 
        case model.state of 
          Dragging _ -> { model | state = Dragging ( toGen x ) } -- set up for 10 generations
          _ -> model
      StopDrag -> 
        case model.state of 
          Dragging generation -> { model | state = Waiting (toFloat <| round generation) } -- set up for 10 generations
          _ -> model
      StartAnimating ->
        case model.state of 
          Waiting generation -> { model | state = Animating (if generation > 7.9 then 0 else generation) } -- set up for 10 generations
          _ -> model
      StopAnimating ->
        case model.state of 
          Animating generation -> { model | state = Waiting generation } -- set up for 10 generations
          _ -> model

-- convert mouse position to generation number and vice versa
toGen mouseX = 
  let
    raw = 0.1 * (mouseX + 50) 
  in
    if raw < 0 then 
      0
    else if raw > 10 then
      10
    else 
      raw
      
toPos genNum = 10 * genNum - 50

sierpinski nMax n c =
  group <|
    ( square 30 |> filled c
    )
    ::
    ( if n > 1 then
        let
          aChild = sierpinski nMax (n-1) c|> (scale 0.7)
        in
          [ aChild |> rotate(degrees 45) |> move (-14.7,29) 
          , aChild |> rotate(degrees -45) |> move (14.7,29) 
          ]
      else if n > 0 then
        let
          aChild = sierpinski nMax (n-1) c |> (scale 0.7)
                      |> makeTransparent n
        in
          [ aChild |> rotate(degrees 45) |> move (-14.7,29) 
          , aChild |> rotate(degrees -45) |> move (14.7,29)
          ]
      else []
    )

init = { state = Waiting 0
       , time = 0
       }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)





