module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, h2, button, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Gravatar exposing (defaultOptions, img)
import Time exposing (second, Time, inSeconds)
import Maybe
import Random.List
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Mobber =
    { name : String
    , email : String
    }


type alias Model =
    { name : String
    , email : String
    , mobbers : List Mobber
    , mobbersLeft : List Mobber
    , time : Time
    , remaining : Int
    , running : Bool
    , current : Maybe Mobber
    , state : State
    }


roundLength : number
roundLength =
    75


gravatarOptions : Gravatar.Options
gravatarOptions =
    { size = Just 400, default = Gravatar.MonsterID, rating = Gravatar.RatedX, forceDefault = False }


defaultUser : Mobber
defaultUser =
    Mobber "Ingen" "ingen@ingen.com"


type State
    = Enter
    | Running
    | Selecting


init : ( Model, Cmd Msg )
init =
    { name = "", email = "", mobbers = [], mobbersLeft = [], time = 0, remaining = roundLength, running = False, current = Nothing, state = Enter } => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every second Tick
    else
        Sub.none



-- UPDATE


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


type Msg
    = ChangeName String
    | ChangeEmail String
    | AddMobber
    | Start
    | Tick Time
    | Pause
    | Reset
    | Select ( Maybe Mobber, List Mobber )
    | Next


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeName newName ->
            { model | name = newName } => Cmd.none

        ChangeEmail newEmail ->
            { model | email = newEmail } => Cmd.none

        AddMobber ->
            let
                m =
                    Mobber model.name model.email
            in
                { model | mobbers = (m :: model.mobbers), name = "", email = "" } => Cmd.none

        Tick time ->
            let
                ru =
                    if model.remaining <= 0 then
                        False
                    else
                        True

                re =
                    if model.remaining <= 0 then
                        roundLength
                    else
                        model.remaining - 1

                st =
                    if model.remaining <= 0 then
                        Selecting
                    else
                        Running

                cu =
                    if model.remaining <= 0 then
                        Random.generate Select (Random.List.choose model.mobbersLeft)
                    else
                        Cmd.none
            in
                { model | remaining = re, running = ru, state = st } => cu

        Start ->
            { model | running = True, state = Running } => Cmd.none

        Pause ->
            { model | running = False } => Cmd.none

        Reset ->
            { model | remaining = roundLength } => Cmd.none

        Select ( mob, mobbersLeft ) ->
            let
                ml =
                    if List.isEmpty mobbersLeft then
                        model.mobbers
                    else
                        mobbersLeft
            in
                { model | current = mob, state = Selecting, mobbersLeft = ml } => Cmd.none

        Next ->
            model => Random.generate Select (Random.List.choose model.mobbers)



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        Enter ->
            div []
                [ div []
                    [ h2 [] [ text "Deltagare" ]
                    , deltagarlista model.mobbers
                    , inputform model
                    , button [ onClick Next, disabled model.running ] [ text "Starta!" ]
                    ]
                ]

        Running ->
            div []
                [ deltagare (Maybe.withDefault defaultUser model.current)
                , button [ onClick Start, disabled model.running ] [ text "Starta!" ]
                , button [ onClick Pause, disabled (not model.running) ] [ text "Pausa" ]
                , button [ onClick Reset ] [ text "Återställ" ]
                , div [] [ text (model.remaining |> toMinutesAndSeconds) ]
                ]

        Selecting ->
            div []
                [ deltagare (Maybe.withDefault defaultUser model.current)
                , div [] [ button [ onClick Start ] [ text "Starta!" ] ]
                ]


toMinutesAndSeconds : Int -> String
toMinutesAndSeconds t =
    let
        m =
            (t // 60) |> toString

        s =
            (rem t 60)

        ss =
            if s < 10 then
                "0" ++ (toString <| s)
            else
                s |> toString
    in
        m ++ ":" ++ ss


deltagare : Mobber -> Html Msg
deltagare mobber =
    div []
        [ img gravatarOptions mobber.email
        , span [] [ text mobber.name ]
        ]


deltagarlista : List Mobber -> Html Msg
deltagarlista mobbers =
    div [] (List.map deltagare mobbers)


inputform : Model -> Html Msg
inputform model =
    div []
        [ input [ placeholder "Namn", onInput ChangeName, value model.name ] []
        , input [ placeholder "E-post", onInput ChangeEmail, value model.email ] []
        , button [ onClick AddMobber ] [ text "+" ]
        ]
