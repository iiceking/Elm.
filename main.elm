module Main exposing (..)

import Browser
import Html exposing (..)
import Http
import Random
import List
import Html.Attributes exposing (type_, placeholder, value, style)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (list, string, Decoder, field)

-- MODEL

type Model
    = Loading
    | Ready (List String)
    | ShowWord String (List String)
    | Error String

init : () -> (Model, Cmd Msg)
init _ =
    ( Loading
    , Http.get
        { url = "https://perso.liris.cnrs.fr/tristan.roussillon/GuessIt/thousand_words_things_explainer.txt"
        , expect = Http.expectString GotText
        }
    )

type Msg
    = GotText (Result Http.Error String)
    | ShowDefinitions (Result Http.Error (List String))
    | PickWord

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText (Ok text) ->
            let
                words = List.filter (\w -> String.length w > 0) (String.split "\n" text)
            in
            ( Ready words, Cmd.none )

        GotText (Err error) ->
            ( Error (Http.errorToString error), Cmd.none )

        PickWord ->
            case model of
                Ready words ->
                    let
                        pickRandomWord = Random.generate (\word -> ShowDefinitions (Ok [word])) (Random.element words)
                    in
                    ( model, pickRandomWord )

                _ ->
                    ( model, Cmd.none )

        ShowDefinitions (Ok [word]) ->
            let
                fetchDefs = Http.get
                    { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ word
                    , expect = Http.expectJson (decodeDefs word) definitionsDecoder
                    }
            in
            ( model, fetchDefs )

        ShowDefinitions (Err error) ->
            ( Error (Http.errorToString error), Cmd.none )

        ShowDefinitions (Ok defs) ->
            ( ShowWord word defs, Cmd.none )

definitionsDecoder : Decoder (List String)
definitionsDecoder =
    list (field "definition" string)

decodeDefs : String -> Decoder Msg
decodeDefs word =
    field "meanings" (list (field "definitions" (list (field "definition" string)))) |> Json.Decode.map (\defs -> ShowDefinitions (Ok defs))

-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text "Loading..."

        Ready _ ->
            button [ onClick PickWord ] [ text "Pick a Word" ]

        ShowWord word defs ->
            div []
                [ h3 [] [ text ("Word: " ++ word) ]
                , ul [] (List.map (\def -> li [] [ text def ]) defs)
                ]

        Error errorMessage ->
            div [] [ text ("Error: " ++ errorMessage) ]

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
