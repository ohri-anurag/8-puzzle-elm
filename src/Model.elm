module Model exposing
    ( Operation(..)
    , Model
    , initModel
    )


import List exposing (range)
import Random exposing (generate)
import Random.List exposing (shuffle)


type Operation
  = Up
  | Down
  | Left
  | Right
  | NoOp
  | ShuffledList (List Int)
  | Undo
  | Reset
  | NewPuzzle


-- MODEL
type alias Model =
  { board: Board
  , moves: Int
  , history: List Board
  }


type alias Board =
  { entries: List Int
  , zero: Int
  }


initModel : () -> (Model, Cmd Operation)
initModel _ =
  let
    list = range 0 8
    board =
      { entries = list
      , zero = 8
      }
  in
  ( { board = board
    , moves = 0
    , history = []
    }
  , generate ShuffledList (shuffle list)
  )