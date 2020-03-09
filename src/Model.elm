module Model exposing (..)


import List exposing (range)
import Random exposing (generate)
import Random.List exposing (shuffle)


type Operation
  = IntroAction IAClick
  | Game Movement
  | Button Click
  | Command Action
  | NoOp


type IAClick
  = FirstPageDone
  | SecondPageDone
  | LastPageDone


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
type Model
  = Introduction Page
  | State AppState


type Page
  = FirstPage
  | SecondPage
  | LastPage


type alias AppState =
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
  ( Introduction FirstPage
  , Cmd.none
  )