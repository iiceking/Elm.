module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (value, placeholder)
import Html.Events exposing (onInput, onClick)
import Http
import Random
import Json.Decode exposing (Decoder, field, string, list, at)

-- MODEL

type Model
    = Loading
    | Ready (List String) String
    | ShowDefinitions String (List String) String
    | Error String
    | Guessing String String (List String) -- Added for guessing

type alias Definition =
    { definition : String }

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
    | InputGuess String
    | SubmitGuess

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText (Ok text) ->
            let
                words = List.filter (\w -> String.length w > 0) (String.split " " text)
            in
            ( Ready words "", Cmd.none )

        GotText (Err error) ->
            ( Error (Http.errorToString error), Cmd.none )

        PickWord ->
            case model of
                Ready words _ ->
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
            ( Loading, fetchDefs )

        ShowDefinitions (Err error) ->
            ( Error (Http.errorToString error), Cmd.none )

        ShowDefinitions (Ok defs) ->
            case model of
                Ready _ _ ->
                    ( Guessing word "" defs, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InputGuess guess ->
            case model of
                Guessing word _ defs ->
                    ( Guessing word guess defs, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SubmitGuess ->
            case model of
                Guessing word guess defs ->
                    if guess == word then
                        ( Ready [], Cmd.none ) -- Correct Guess, Reset or Show Success Message
                    else
                        ( Guessing word guess defs, Cmd.none ) -- Incorrect Guess, Try Again

                _ ->
                    ( model, Cmd.none )

-- DECODERS

definitionsDecoder : Decoder (List String)
definitionsDecoder =
    at ["meanings"] (list (at ["definitions"] (list (field "definition" string))))

decodeDefs : String -> Decoder Msg
decodeDefs word =
    definitionsDecoder
        |> Json.Decode.map (\defs -> ShowDefinitions (Ok defs))

-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text "Loading..."

        Ready _ _ ->
            div []
                [ button [ onClick PickWord ] [ text "Pick a Word" ]
                ]

        Guessing word guess defs ->
            div []
                [ h3 [] [ text "Definitions" ]
                , ul [] (List.map (\def -> li [] [ text def ]) defs)
                , input [ placeholder "Enter your guess", onInput InputGuess, value guess ] []
                , button [ onClick SubmitGuess ] [ text "Submit Guess" ]
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
