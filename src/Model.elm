module Model exposing
    ( Action(..)
    , Click(..)
    , Operation(..)
    , Model
    , Movement(..)
    , initModel
    )


import List exposing (range)
import Random exposing (generate)
import Random.List exposing (shuffle)


type Operation
  = Game Movement
  | Button Click
  | Command Action
  | NoOp



type Movement
  = Up
  | Down
  | Left
  | Right


type Click
  = Undo
  | Reset
  | NewPuzzle


type Action
  = ShuffledList (List Int)

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
  , shuffle list
      |> generate (ShuffledList >> Command)
  )