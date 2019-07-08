module Main exposing (main)

import Browser
import Html exposing (..)



-- Model


type alias Model =
    String


initialModel : Model
initialModel =
    "Welcome to Elm Chess!"



-- Update


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model



-- View


view : Model -> Html Msg
view model =
    div []
        [ text model ]



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }
