module Update exposing
    ( update
    )


import List exposing (filter, indexedMap, length, map, range, sum)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Tuple exposing (pair)

import Helper exposing (last, tails)
import Model exposing (Model, Operation(..))


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
        | board =
          { entries = newList
          , zero = findZeroIndex newList
          }
        , moves = 0
        , history = []
        }
      , Cmd.none
      )

    Undo ->
      case model.history of
        (b :: bs) ->
          (
            { model
            | board = b
            , moves = model.moves - 1
            , history = bs
            }
          , Cmd.none
          )

        [] ->
          (model, Cmd.none)

    Reset ->
      case last model.history of
        Nothing ->
          (model, Cmd.none)

        Just b ->
          (
            { model
            | board = b
            , moves = 0
            , history = []
            }
          , Cmd.none
          )

    NewPuzzle ->
      ( model
      , range 0 8
          |> shuffle
          |> generate ShuffledList
      )


updateModel : Int -> Model -> Model
updateModel movement model =
  let
    newZero = model.board.zero + movement
    newBoard =
      { entries = swap model.board.zero newZero model.board.entries
      , zero = newZero
      }
  in
  { model
  | board = newBoard
  , moves = model.moves + 1
  , history = model.board :: model.history
  }


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