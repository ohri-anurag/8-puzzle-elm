module View exposing
    (view)


import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import List exposing (drop, indexedMap, isEmpty, map, take)
import String exposing (fromInt)
import Tuple exposing (pair)


import Model exposing (Model, Operation(..))


view : Model -> Html Operation
view model =
  let
    puzzle =
      model.board.entries
        |> indexedMap pair
        |> rowify model.board.zero
        |> div [attribute "class" "puzzle"]

    newButton =
      div
        [ attribute "class" "new"
        , onClick NewPuzzle]
        [
          span [attribute "class" "button"] [text "New Puzzle"]
        ]

    undoResetButton =
      div [attribute "class" "undoReset"]
        [ div [attribute "class" "button", onClick Undo] [text "Undo"]
        , div [attribute "class" "button", onClick Reset] [text "Reset"]
        ]
  in
  div [attribute "class" "app"]
    [ div [attribute "class" "boardAndButtons"]
      [ newButton
      , puzzle
      , undoResetButton
      ]
    , div [attribute "class" "moves"] [fromInt model.moves |> (++) "Moves: " |> text]
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
