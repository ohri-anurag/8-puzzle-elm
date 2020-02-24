module Main exposing(..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = initModel, update = update, view = view }

type Msg = Increment | Decrement

initModel = [1,2,3,4,5,6,7,8,0]

update msg model = model

view model = div []
  [
  div []
    [ div [] [ text "1" ]
    , div [] [ text "2" ]
    , div [] [ text "3" ]
    ],
  div []
    [ div [] [ text "1" ]
    , div [] [ text "2" ]
    , div [] [ text "3" ]
    ],
  div []
    [ div [] [ text "1" ]
    , div [] [ text "2" ]
    , div [] [ text "3" ]
    ]
  ]