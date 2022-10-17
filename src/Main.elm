module Main exposing (..)

import Browser
import Browser.Events as E
import Html.Events exposing (onClick)
import Html exposing (..)
import Html.Attributes as A
import Maybe exposing (withDefault)
import Json.Decode as D
import Svg
import Svg.Attributes exposing (..)

--------------------------------------------------------------------------------
{- MAIN -}
--------------------------------------------------------------------------------
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


--------------------------------------------------------------------------------
{- MODEL -}
--------------------------------------------------------------------------------
-- Initial model
init : () -> (Model, Cmd Msg)
init _ =
  ( { mousePos = { x = 0, y = 0 }
    , lines = 
        [ makeLine 1 1 boxHeight 90
        , makeLine 1 0 boxWidth 180
        , makeLine 0 0 boxHeight 270
        , makeLine 0 1 boxWidth 0
        , makeLine 0.75 0.75 (1/4*boxHeight) 30
        , makeLine 0.75 0.25 (1/3*boxHeight) 130
        , makeLine 0.25 0.25 (1/3*boxHeight) 270
        , makeLine 0.25 0.75 (1/3*boxWidth) 0
        , makeLine 0.5 0.95 (1/3*boxWidth) 145
        , makeLine 0.1 0.3 (1/4*boxHeight) 35
        , makeLine 0.7 0.5 (0.4*boxWidth) 165
        ]
    , rays = []
    , rayVisibility = False
    }
  , Cmd.none
  )

makeLine : Float -> Float -> Float -> Float -> Line
makeLine startX startY length angle = -- startX, startY are proportions of boxWidth & boxHeight
  { initPt = { x = boxWidth * startX, y = boxHeight * startY }, angle = angle, length = length}

boxWidth : Float
boxWidth = 500
boxHeight : Float
boxHeight = 500
widthMargin : Float
widthMargin = 0
heightMargin : Float
heightMargin = 0

type alias Model = 
  { mousePos : Point
  , lines : List Line
  , rays : List Line
  , rayVisibility : Bool
  }

type alias Point = 
  { x: Float
  , y: Float
  }

type alias Line = 
  { initPt : Point
  , angle : Float -- in degrees
  , length : Float
  }

-- Return a line's starting point's x,y-coordinates as a tuple
getXY : Line -> (Float, Float)
getXY line =
  Tuple.pair line.initPt.x line.initPt.y

-- Return a line's unit direction vector's x,y-coordinates as a tuple
getXYdir : Line -> (Float, Float)
getXYdir line =
  Tuple.pair (cos (degrees line.angle)) (sin (degrees line.angle))

-- Return end point of given line
lineFinalPt : Line -> Point
lineFinalPt line =
  let
    (x1, y1) = (getXY line)
    (dx, dy) =  (getXYdir line)
    len = line.length
  in
    {x = toFloat (round (x1 + len*dx)), y = toFloat (round (y1 - len*dy))}

linesParallel : Line -> Line -> Bool
linesParallel line1 line2 =
  (cos (degrees line1.angle) == cos (degrees line2.angle)) || 
  (sin (degrees line1.angle) == sin (degrees line2.angle)) 

-- Return the angle (direction) of the line with its end points being the given two points
twoPtsAngle : Point -> Point -> Float
twoPtsAngle initPt finalPt =
  let
    dx = finalPt.x - initPt.x
    dy = -(finalPt.y - initPt.y)
  in
    (atan2 dy dx) * 180 / pi


--------------------------------------------------------------------------------
{- UPDATE -}
--------------------------------------------------------------------------------
type Msg 
  = MouseMove Point | RayVisibilityToggle

-- Return updated model given a message
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMove position -> 
      ( { model | mousePos = constrainPos position, 
          rays = (updateRays position model.lines) 
        }
      , Cmd.none
      )
    RayVisibilityToggle ->
      ( { model | rayVisibility = (not model.rayVisibility) }, Cmd.none)
    
-- Constrain given point within display box
constrainPos : Point -> Point
constrainPos pt =
  let
    newX = constrainFloat pt.x widthMargin boxWidth + widthMargin
    newY = constrainFloat pt.y heightMargin boxHeight + heightMargin
  in
    { pt | x = newX, y = newY }
  
constrainFloat : Float -> Float -> Float -> Float
constrainFloat val min max =
  if val < min then min
  else if val > max then max
  else val

updateRays : Point -> List Line -> List Line
updateRays mousePos lines =
  let
    -- Create a list of all end points of given lines
    linePts = List.concat [(List.map .initPt lines), (List.map lineFinalPt lines)]
    -- Create a list of the angle of the ray from mousePos to each point in linePts
    rayAngles =  ((List.map (twoPtsAngle mousePos) linePts))
    -- Create a list of rays (lines) with angles corresponding to each of rayAngles & its +/-0.005 offset
    newRays = List.concat (List.map (getOffsetRays 0.5 mousePos) rayAngles)
    -- Get new ray lengths by finding nearest intersection with a line 
    newLengths = List.map (nearestIntersect lines) newRays
  in
    List.sortBy .angle (List.map2 updateLength newLengths newRays)
    
getOffsetRays : Float -> Point -> Float -> List Line
getOffsetRays offSet initPt angle =
  [ { initPt = initPt, angle = angle - offSet, length = 0}
  , { initPt = initPt, angle = angle - offSet * 1/2, length = 0}
  , { initPt = initPt, angle = angle, length = 0}
  , { initPt = initPt, angle = angle + offSet * 1/2, length = 0}
  , { initPt = initPt, angle = angle + offSet, length = 0}
  ]

updatePoint : Point -> Line -> Line
updatePoint pt line =
  { line | initPt = pt}

updateLength : Float -> Line -> Line
updateLength len line =
  { line | length = len }

-- Find the shortest length of ray that intersects with one of the given lines 
-- in the direction of given ray and with same starting point
nearestIntersect :  List Line -> Line -> Float
nearestIntersect lines ray = 
  lines 
    -- Find length of ray if given ray and line intersects
    |> List.map (findIntersect ray)
    -- Remove Nothing values (i.e. remove non-intersections)
    |> List.filterMap identity
    |> List.minimum
    |> withDefault 0

-- Find length of ray to its intersection with given line, if intersection exists
-- Else, return Nothing
findIntersect : Line -> Line -> Maybe Float
findIntersect ray line = 
  if (linesParallel ray line) then
    Nothing
  else if List.member (Tuple.second (getXYdir ray)) [-1, 1] then
    findVerticalRayIntersect ray line
  else
    findNonVerticalRayIntersect ray line

findNonVerticalRayIntersect : Line -> Line -> Maybe Float
findNonVerticalRayIntersect ray line =
  let
    (r_px, r_py) = getXY ray
    (r_dx, r_dy) = getXYdir ray
    (s_px, s_py) = getXY line
    (s_dx, s_dy) = getXYdir line
    t2 = Debug.log "t2" ((r_dx * (s_py-r_py) - r_dy * (r_px-s_px)) / (-s_dx * r_dy + s_dy * r_dx))
    t1 =  Debug.log "t1" ((s_px + s_dx * t2 - r_px) / r_dx )-- Potential length of ray to intersection with line
  in
    Debug.log "validIntersect" (validIntersect (toFloat (round t1)) (toFloat (round t2)) line.length)

findVerticalRayIntersect : Line -> Line -> Maybe Float
findVerticalRayIntersect ray line = 
  let
    (r_px, r_py) = getXY ray
    (r_dx, r_dy) = getXYdir ray
    (s_px, s_py) = getXY line
    (s_dx, s_dy) = getXYdir line
    t1 =  ((s_dx * (r_py-s_py) - s_dy * (s_px-r_px)) / (-r_dx * s_dy + r_dy * s_dx))
    t2 =  ((r_px + r_dx * t1 - s_px) / s_dx)
  in
    validIntersect (toFloat (round t1)) (toFloat (round t2)) line.length

-- If given t1 & t2 imply a valid intersection was found, return t1 
-- (i.e. length of ray to intersection with line). Else, return Nothing.
validIntersect : Float -> Float -> Float -> Maybe Float
validIntersect t1 t2 len =
  if ( (Debug.log "valid intersect bool 1a" (isNaN t2)) || (Debug.log "valid intersect bool 1b" (t2 > len)) || (Debug.log "valid intersect bool 1c" (t2 < 0))) then
    Nothing
  else if Debug.log "valid intersect bool 2" (t1 <= 0) then
    Nothing
  else
    Just t1


--------------------------------------------------------------------------------
{- VIEW -}
--------------------------------------------------------------------------------
-- Given model, return HTML page
view : Model -> Html Msg
view model =
  div [ A.style "font-family" "Courier New", A.style "width" "500px"] [
    section [] 
      [ Svg.svg 
        [ width (String.fromFloat boxWidth) , height (String.fromFloat boxHeight)]
        ( List.concat 
          [ [drawVisibilityPolygon model.rays] 
          , (if model.rayVisibility 
                then (List.map (drawLine "palegoldenrod" "1.5") model.rays) 
                else [] 
            )
          , (List.map (drawLine "midnightblue" "3") model.lines) 
          ]
        )
      ]
    , section [] 
        [ button [ onClick RayVisibilityToggle ] [ text "Toggle Ray Visibility" ] ]
    , section [] 
        [ h1 [] [ text "Shadow Animation in Elm" ]
        , div [] 
            [ text "Instructions:"
            , ul [] 
                [ li [] [text "Try to move cursor around box above to show visible areas (colored) and shadows (no color)"]
                , li [] [text "Click on \"Toggle Ray Visibility\" and move cursor in the box to see how rays are casted"]
                ]
            ]
        , div [ A.style "margin-top" "50px"]
            [ text "Source code: "
            , a [ A.href "https://github.com/youngjol/shadow-animation" ] [ text "Github" ]
            ]
        , div []
            [ text "Based on: "
            , a [ A.href "https://ncase.me/sight-and-light/" ] [ text "tutorial" ]
            ]
        ]
    ]

drawLine : String -> String -> Line -> Svg.Svg msg
drawLine color width line =
    Svg.line
    [ x1 (String.fromFloat line.initPt.x)
    , y1 (String.fromFloat line.initPt.y)
    , x2 (String.fromFloat (lineFinalPt line).x)
    , y2 (String.fromFloat (lineFinalPt line).y)
    , stroke color
    , strokeWidth width]
    []

drawVisibilityPolygon : List Line -> Svg.Svg msg
drawVisibilityPolygon rays =
  Svg.polygon 
  [ points
      (rays 
        |> List.map lineFinalPt -- get end points of given list of lines
        |> List.map pointAsString
        |> String.concat
      )
  , fill "lightsteelblue"
  ]
  []

pointAsString : Point -> String
pointAsString pt =
  (String.fromInt (round pt.x)) ++ "," ++ (String.fromInt (round pt.y) ++ " ")


--------------------------------------------------------------------------------
{- SUBSCRIPTIONS -}
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model =
  E.onMouseMove (D.map MouseMove decodePoint)

decodePoint : D.Decoder Point
decodePoint =
  D.map2 Point decodeX decodeY

decodeX : D.Decoder Float
decodeX =
  D.field "clientX" D.float

decodeY : D.Decoder Float
decodeY =
  D.field "pageY" D.float