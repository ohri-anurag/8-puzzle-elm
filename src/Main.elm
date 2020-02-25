module Main exposing(..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import List exposing ((::), drop, isEmpty, map, take)
import String exposing (fromInt)

main =
  Browser.sandbox { init = initModel, update = update, view = view }

type Msg = Increment | Decrement

type alias Model =
  { model: List Int
  , zero: Int
  }

initModel : Model
initModel =
  { model = [1,2,3,4,5,6,7,8,0]
  , zero = 8
  }

update msg model = model

view model = div [] (viewify model.model)

viewify list =
  if isEmpty list
    then []
    else div [] (map divify (take 3 list)) :: viewify (drop 3 list)

divify num = div [
    if num == 0
      then attribute "class" "zero"
      else attribute "class" "cell"
  ] [ text (fromInt num) ]
