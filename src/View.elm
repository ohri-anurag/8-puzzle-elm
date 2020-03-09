module View exposing
    (view)


import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import List exposing (drop, indexedMap, isEmpty, map, take)
import String exposing (fromInt)
import Tuple exposing (pair)


import Model exposing (..)


view : Model -> Html Operation
view model =
  case model of
    Introduction page ->
      viewPage page

    State appState ->
      viewState appState


viewPage : Page -> Html Operation
viewPage page =
  case page of
    FirstPage ->
      div [attribute "class" "firstPage"]
        [
          span
            [ attribute "class" "button"
            , onClick (IntroAction FirstPageDone)
            ] [text "8 Puzzle"]
        ]

    SecondPage ->
      let
        images =
          div [attribute "class" "images"]
            [ img [attribute "alt" "Unsolved Puzzle", attribute "src" "img/Puzzle.png"] []
            , img [attribute "alt" "After Solving", attribute "src" "img/Arrow.png"] []
            , img [attribute "alt" "solved Puzzle", attribute "src" "img/Solved.png"] []
            ]

        helperText =
          div [attribute "class" "helperText"]
            [ text "The aim of this game is to convert the original puzzle to the solved version."
            ]

        nextButton =
          div
            [ attribute "class" "nextButton button"
            , onClick (IntroAction SecondPageDone)
            ] [text "Next"]
      in
      div [attribute "class" "secondPage"]
        [ div [attribute "class" "imagesDiv"]
          [ images
          , helperText
          , nextButton
          ]
        ]

    LastPage ->
      let
        image =
          div [attribute "class" "images"]
            [ img [attribute "alt" "Unsolved Puzzle", attribute "src" "img/Candidates.png"] []]

        helperText =
          div [attribute "class" "helperText"]
            [ text 
              """
              At every move, some cells will be highlighted in green. 
              You can click on one of these cells to swap this with the zero cell which is in gray.
              """
            ]

        nextButton =
          div
            [ attribute "class" "nextButton button"
            , onClick (IntroAction LastPageDone)
            ] [text "Finish"]
      in
      div [attribute "class" "lastPage"]
        [ div [attribute "class" "imagesDiv"]
          [ image
          , helperText
          , nextButton
          ]
        ]

viewState : AppState -> Html Operation
viewState appState =
  let
    puzzle =
      appState.board.entries
        |> indexedMap pair
        |> rowify appState.board.zero
        |> div [attribute "class" "puzzle"]

    newButton =
      div
        [ attribute "class" "new"
        , onClick (NewPuzzle |> Button)]
        [
          span [attribute "class" "button"] [text "New Puzzle"]
        ]

    undoResetButton =
      div [attribute "class" "undoReset"]
        [ div [attribute "class" "button", onClick (Undo |> Button)] [text "Undo"]
        , div [attribute "class" "button", onClick (Reset |> Button)] [text "Reset"]
        ]

    movesText =
      if hasPlayerWon appState.board.entries
        then "You Won! Moves: "
        else "Moves: "
  in
  div [attribute "class" "app"]
    [ div [attribute "class" "boardAndButtons"]
      [ newButton
      , puzzle
      , undoResetButton
      ]
    , div
        [ attribute "class" <|
            if hasPlayerWon appState.board.entries
              then "moves finished"
              else "moves"
        ]
        [ fromInt appState.moves
            |> (++) movesText
            |> text
        ]
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
        then whichNeighbour pos zeroPos |> Game
        else NoOp
  ] [
    span [attribute "class" "centreSpan"] [fromInt num |> text]
  ]


isNeighbour : Int -> Int -> Bool
isNeighbour pos1 pos2 =
  abs(pos1 - pos2) == 1 || abs(pos1 - pos2) == 3


whichNeighbour : Int -> Int -> Movement
whichNeighbour pos zeroPos =
  if pos - zeroPos == -3
    then Up
    else if pos - zeroPos == -1
      then Left
      else if pos - zeroPos == 1
        then Right
        else Down


hasPlayerWon : List Int -> Bool
hasPlayerWon list = list == [1,2,3,4,5,6,7,8,0]