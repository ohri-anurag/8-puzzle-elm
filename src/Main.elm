module Main exposing(..)

import Basics exposing ((|>))
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import List exposing ((::), drop, indexedMap, isEmpty, map, sum, take)
import String exposing (fromInt)
import Tuple exposing (pair)

main =
  Browser.sandbox { init = initModel, update = update, view = view }


type Operation = Up | Down | Left | Right | NoOp


type alias Model =
  { model: List Int
  , zero: Int
  }


initModel : Model
initModel =
  { model = [1,2,3,4,5,6,7,8,0]
  , zero = 8
  }


update : Operation -> Model -> Model
update msg model =
  case msg of
    Up ->
      updateModel (-3) model

    Down ->
      updateModel 3 model

    Left ->
      updateModel (-1) model

    Right ->
      updateModel 1 model

    NoOp ->
      model


updateModel : Int -> Model -> Model
updateModel movement model =
  let
    newZero = model.zero + movement
  in
  { model
  | model = swap model.zero newZero model.model
  , zero = newZero
  }


swap : Int -> Int -> List Int -> List Int
swap i j list =
  let
    swapSum =
      list
        |> indexedMap
          (\k n ->
            if k == i || k == j
              then n
              else 0
          )
        |> sum
  in
  list
    |> indexedMap
      (\k n ->
        if k == i || k == j
          then swapSum - n
          else n
      )

view : Model -> Html Operation
view model =
  model.model
    |> indexedMap pair
    |> rowify model.zero
    |> div []


rowify : Int -> List (Int, Int) -> List (Html Operation)
rowify zeroPos list =
  if isEmpty list
    then []
    else div [] (map (divify zeroPos) (take 3 list)) :: rowify zeroPos (drop 3 list)


divify : Int -> (Int, Int) -> Html Operation
divify zeroPos (pos, num) =
  div [
    attribute "class" <|
      if num == 0
        then "zero"
        else "cell",
    onClick <|
      if isNeighbour pos zeroPos
        then whichNeighbour pos zeroPos
        else NoOp
  ] [ text (fromInt num) ]


isNeighbour : Int -> Int -> Bool
isNeighbour pos1 pos2 =
  abs(pos1 - pos2) == 1 || abs(pos1 - pos2) == 3


whichNeighbour : Int -> Int -> Operation
whichNeighbour pos zeroPos =
  if pos - zeroPos == -3
    then Up
    else if pos - zeroPos == -1
      then Left
      else if pos - zeroPos == 1
        then Right
        else Down

