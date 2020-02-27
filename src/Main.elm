module Main exposing(..)

import Basics exposing ((|>))
import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import List exposing ((::), drop, filter, indexedMap, isEmpty, length, map, range, sum, take)
import Random exposing (generate)
import Random.List exposing (shuffle)
import String exposing (fromInt)
import Tuple exposing (pair)


--MAIN
main =
  Browser.element
    { init = initModel
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type Operation
  = Up
  | Down
  | Left
  | Right
  | NoOp
  | ShuffledList (List Int)


-- MODEL
type alias Model =
  { model: List Int
  , zero: Int
  , moves: Int
  }


initModel : () -> (Model, Cmd Operation)
initModel _ =
  let
    list = range 0 8
  in
  ( { model = list
    , zero = 8
    , moves = 0
    }
  , generate ShuffledList (shuffle list)
  )


-- UPDATE
update : Operation -> Model -> (Model, Cmd Operation)
update msg model =
  case msg of
    Up ->
      (updateModel (-3) model, Cmd.none)

    Down ->
      (updateModel 3 model, Cmd.none)

    Left ->
      (updateModel (-1) model, Cmd.none)

    Right ->
      (updateModel 1 model, Cmd.none)

    NoOp ->
      (model, Cmd.none)

    ShuffledList list ->
      let
        newList = swapIfNecessary list
      in
      (
        { model
        | model = newList
        , zero = findZeroIndex newList}
      , Cmd.none
      )


swapIfNecessary : List Int -> List Int
swapIfNecessary list =
  let
    zeroPos = findZeroIndex list
  in
  if isSolvable list
    then list
    else if zeroPos > 1
      then swap 0 1 list
      else if zeroPos == 0
        then swap 1 2 list
        else swap 0 2 list


isSolvable : List Int -> Bool
isSolvable list =
  let
    inversions (x, xs) =
      filter ((>) x) xs
        |> length

    totalInversions =
      filter ((<) 0) list
        |> tails
        |> map inversions
        |> sum
  in
  modBy totalInversions 2 == 0


tails : List a -> List (a, List a)
tails xs =
  case xs of
    [] ->
      []

    (x::rest) ->
      (x, rest) :: tails rest


findZeroIndex : List Int -> Int
findZeroIndex list =
  let
    helper xs =
      case xs of
        [] ->
          0

        ((a,b)::rest) ->
          if b == 0
            then a
            else helper rest

  in
  indexedMap pair list
    |> helper


updateModel : Int -> Model -> Model
updateModel movement model =
  let
    newZero = model.zero + movement
  in
  { model
  | model = swap model.zero newZero model.model
  , zero = newZero
  , moves = model.moves + 1
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


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Operation
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Html Operation
view model =
  let
    puzzle =
      model.model
        |> indexedMap pair
        |> rowify model.zero
        |> div []
  in
  div [attribute "class" "app"] [
    div [attribute "class" "boardAndButtons"] [puzzle],
    div [attribute "class" "moves"] [fromInt model.moves |> (++) "Moves: " |> text]
  ]


rowify : Int -> List (Int, Int) -> List (Html Operation)
rowify zeroPos list =
  if isEmpty list
    then []
    else
      let
        row = div [attribute "class" "row"] (map (divify zeroPos) (take 3 list))
      in
      row :: rowify zeroPos (drop 3 list)


divify : Int -> (Int, Int) -> Html Operation
divify zeroPos (pos, num) =
  div [
    attribute "class" <|
      if num == 0
        then "zero"
        else if isNeighbour pos zeroPos
          then "neighbour"
          else "cell",
    onClick <|
      if isNeighbour pos zeroPos
        then whichNeighbour pos zeroPos
        else NoOp
  ] [
    span [attribute "class" "centreSpan"] [fromInt num |> text]
  ]


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
