module Update exposing
    ( update
    )


import List exposing (filter, indexedMap, length, map, range, sum)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Tuple exposing (pair)

import Helper exposing (last, tails)
import Model exposing (..)


update : Operation -> Model -> (Model, Cmd Operation)
update msg model =
  case model of
    Introduction page ->
      case msg of
        IntroAction iaClick ->
          updatePage iaClick page

        _ ->
          (model, Cmd.none)

    State appState ->
      case msg of
        Game movement ->
          updateMovement movement appState

        Button click ->
          updateClick click appState

        Command action ->
          updateAction action appState

        _ ->
          (model, Cmd.none)


updatePage : IAClick -> Page -> (Model, Cmd Operation)
updatePage iaClick page =
  case iaClick of
    FirstPageDone ->
      (Introduction SecondPage, Cmd.none)

    SecondPageDone ->
      (Introduction LastPage, Cmd.none)

    LastPageDone ->
      let
        list = range 0 8
      in
      ( State
        { board =
          { entries = list
          , zero = 0
          }
        , moves = 0
        , history = []
        }
      , shuffle list
          |> generate (ShuffledList >> Command)
      )


updateMovement : Movement -> AppState -> (Model, Cmd Operation)
updateMovement movement appState =
  let
    updateState movementIndex =
      let
        newZero = appState.board.zero + movementIndex
        newBoard =
          { entries = swap appState.board.zero newZero appState.board.entries
          , zero = newZero
          }
      in
      { appState
      | board = newBoard
      , moves = appState.moves + 1
      , history = appState.board :: appState.history
      } |> State
  in
  case movement of
    Up ->
      (updateState (-3), Cmd.none)

    Down ->
      (updateState 3, Cmd.none)

    Left ->
      (updateState (-1), Cmd.none)

    Right ->
      (updateState 1, Cmd.none)


updateClick : Click -> AppState -> (Model, Cmd Operation)
updateClick click appState =
  case click of
    Undo ->
      case appState.history of
        (b :: bs) ->
          (
            { appState
            | board = b
            , moves = appState.moves - 1
            , history = bs
            } |> State
          , Cmd.none
          )

        [] ->
          (State appState, Cmd.none)

    Reset ->
      case last appState.history of
        Nothing ->
          (State appState, Cmd.none)

        Just b ->
          (
            { appState
            | board = b
            , moves = 0
            , history = []
            } |> State
          , Cmd.none
          )

    NewPuzzle ->
      ( State appState
      , range 0 8
          |> shuffle
          |> generate (ShuffledList >> Command)
      )


updateAction : Action -> AppState -> (Model, Cmd Operation)
updateAction action appState =
  case action of
    ShuffledList list ->
      let
        newList = swapIfNecessary list
      in
      (
        { appState
        | board =
          { entries = newList
          , zero = findZeroIndex newList
          }
        , moves = 0
        , history = []
        } |> State
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
  modBy 2 totalInversions == 0


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