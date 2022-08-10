module Main exposing (..)

import Browser
import Browser.Events as E
import Html exposing (..)
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
        [ { startPt = { x = boxWidth, y = boxHeight }, angle = 90, length = boxHeight}
        , { startPt = { x = boxWidth, y = 0 }, angle = 180, length = boxWidth}
        , { startPt = { x = 0, y = 0 }, angle = 270, length = boxHeight}
        , { startPt = { x = 0, y = boxHeight }, angle = 0, length = boxWidth}
        , { startPt = { x = 3/4*boxWidth, y = 3/4*boxHeight }, angle = 90, length = 1/3*boxHeight}
        , { startPt = { x = 3/4*boxWidth, y = 1/4*boxHeight }, angle = 180, length = 1/3*boxWidth}
        , { startPt = { x = 1/4*boxWidth, y = 1/4*boxHeight }, angle = 270, length = 1/3*boxHeight}
        , { startPt = { x = 1/4*boxWidth, y = 3/4*boxHeight }, angle = 0, length = 1/3*boxWidth}
        ]
    , rays = 
        [ { startPt = { x = boxWidth/2, y = boxHeight/2 }, angle = 0, length = boxWidth/2}
        , { startPt = { x = boxWidth/2, y = boxHeight/2 }, angle = 90, length = boxHeight/2}
        , { startPt = { x = boxWidth/2, y = boxHeight/2 }, angle = 180, length = boxWidth/2}
        , { startPt = { x = boxWidth/2, y = boxHeight/2 }, angle = 270, length = boxHeight/2}
        , { startPt = { x = 1/4*boxWidth, y = 3/4*boxHeight }, angle = 210, length = 1/3*boxWidth}
        ]
    }
  , Cmd.none
  )

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
  }

type alias Point = 
  { x: Float
  , y: Float
  }

type alias Line = 
  { startPt : Point
  , angle : Float -- in degrees
  , length : Float
  }

-- Return a line's starting point's x,y-coordinates as a tuple
getXY : Line -> (Float, Float)
getXY line =
  Tuple.pair line.startPt.x line.startPt.y

-- Return a line's unit direction vector's x,y-coordinates as a tuple
getXYdir : Line -> (Float, Float)
getXYdir line =
  Tuple.pair (cos (degrees line.angle)) (sin (degrees line.angle))

-- Return end point of given line
lineEndPt : Line -> Point
lineEndPt line =
  let
    (x1, y1) = getXY line
    (dx, dy) = getXYdir line
    len = line.length
  in
    {x = x1 + len*dx, y = y1 - len*dy}

linesParallel : Line -> Line -> Bool
linesParallel line1 line2 =
  (cos (degrees line1.angle) == cos (degrees line2.angle)) || 
  (sin (degrees line1.angle) == sin (degrees line2.angle)) 


--------------------------------------------------------------------------------
{- UPDATE -}
--------------------------------------------------------------------------------
type Msg 
  = MouseMove Point

-- Return updated model given a message
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMove position -> 
      ( { model | mousePos = constrainPos position, 
          rays = (updateRays position model.rays model.lines) 
        }
      , Cmd.none
      )

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

updateRays : Point -> List Line -> List Line -> List Line
updateRays mousePos rays lines =
  let
    -- Update rays' startPt to given mousePos
    newRays = List.map (updatePoint mousePos) rays
    -- Get new ray lengths by finding nearest intersection with a line 
    newLengths = List.map (nearestIntersect lines) newRays 
  in  
    List.map2 updateLength newLengths newRays

updatePoint : Point -> Line -> Line
updatePoint pt line =
  { line | startPt = pt}

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
    t2 = (r_dx * (s_py-r_py) - r_dy * (r_px-s_px)) / (-s_dx * r_dy + s_dy * r_dx)
    t1 = (s_px + s_dx * t2 - r_px) / r_dx -- Potential length of ray to intersection with line
  in
    validIntersect t1 t2 line.length

findVerticalRayIntersect : Line -> Line -> Maybe Float
findVerticalRayIntersect ray line = 
  let
    (r_px, r_py) = getXY ray
    (r_dx, r_dy) = getXYdir ray
    (s_px, s_py) = getXY line
    (s_dx, s_dy) = getXYdir line
    t1 = (s_dx * (r_py-s_py) - s_dy * (s_px-r_px)) / (-r_dx * s_dy + r_dy * s_dx)
    t2 = (r_px + r_dx * t1 - s_px) / s_dx 
  in
    validIntersect t1 t2 line.length

-- If given t1 & t2 imply a valid intersection was found, return t1 
-- (i.e. length of ray to intersection with line). Else, return Nothing.
validIntersect : Float -> Float -> Float -> Maybe Float
validIntersect t1 t2 len =
  if isNaN t2 || t2 > len || t2 <= 0 then
    Nothing
  else if t1 <= 0 then
    Nothing
  else
    Just t1


--------------------------------------------------------------------------------
{- VIEW -}
--------------------------------------------------------------------------------
-- Given model, return HTML page
view : Model -> Html msg
view model =
  Svg.svg [ width (String.fromFloat boxWidth) , height (String.fromFloat boxHeight)
          , viewBox ( "0 " ++ "0 " ++ (String.fromFloat (boxWidth)) 
                      ++ " " ++ (String.fromFloat (boxHeight)) )
          , style "red" 
          ]
          ( List.concat [ (List.map drawLine model.lines)
                        , (List.map drawLine model.rays) ]
          )

drawLine : Line -> Svg.Svg msg
drawLine line =
    Svg.line
    [ x1 (String.fromFloat line.startPt.x)
    , y1 (String.fromFloat line.startPt.y)
    , x2 (String.fromFloat (lineEndPt line).x)
    , y2 (String.fromFloat (lineEndPt line).y)
    , stroke "black"]
    []


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