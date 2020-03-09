module Subscriptions exposing
    (subscriptions)


import Model exposing (Model, Operation(..))


subscriptions : Model -> Sub Operation
subscriptions model =
  Sub.none
