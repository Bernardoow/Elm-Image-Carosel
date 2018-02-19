module ImageCarousel exposing (..)

import Html exposing (Html, text, div, button, img, node, i)
import Html.Attributes exposing (src, attribute, class)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Time


---- MODEL ----


type ChangeType
    = Backward
    | Forward


type alias Flags =
    { imagesList : List String
    }


type alias Model =
    { currentPosition : Int
    , images : Dict Int String
    , playing : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model 0 (List.indexedMap (,) flags.imagesList |> Dict.fromList) True, Cmd.none )



---- UPDATE ----


type Msg
    = ChangeImage Time.Time
    | Stop
    | Start
    | StepBackward
    | StepForward


changeCurrentPosition : Int -> Int -> ChangeType -> Int
changeCurrentPosition currentPosition imagesQuantity changeType =
    case changeType of
        Backward ->
            let
                newPosition =
                    currentPosition - 1
            in
                if newPosition < 0 then
                    imagesQuantity - 1
                else
                    newPosition

        Forward ->
            let
                newPosition =
                    currentPosition + 1
            in
                if newPosition >= imagesQuantity then
                    0
                else
                    newPosition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stop ->
            { model | playing = False } ! []

        Start ->
            { model | playing = True } ! []

        ChangeImage tick ->
            let
                currentPosition =
                    changeCurrentPosition model.currentPosition (Dict.size model.images) Forward
            in
                { model | currentPosition = currentPosition } ! []

        StepBackward ->
            let
                currentPosition =
                    changeCurrentPosition model.currentPosition (Dict.size model.images) Backward
            in
                { model | currentPosition = currentPosition } ! []

        StepForward ->
            let
                currentPosition =
                    changeCurrentPosition model.currentPosition (Dict.size model.images) Forward
            in
                { model | currentPosition = currentPosition } ! []



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        currentImage =
            Dict.get model.currentPosition model.images
    in
        div []
            [ stylesheet
            , case currentImage of
                Nothing ->
                    img [] []

                Just image ->
                    img [ src image ] []
            , div []
                [ button [ onClick StepBackward ]
                    [ i [ attribute "aria-hidden" "true", class "fa fa-step-backward" ]
                        []
                    ]
                , if model.playing then
                    button [ onClick Stop ]
                        [ i [ attribute "aria-hidden" "true", class "fa fa-pause" ]
                            []
                        ]
                  else
                    button [ onClick Start ]
                        [ i [ attribute "aria-hidden" "true", class "fa fa-play" ]
                            []
                        ]
                , button [ onClick StepForward ]
                    [ i [ attribute "aria-hidden" "true", class "fa fa-step-forward" ]
                        []
                    ]
                ]
            ]


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
            ]

        children =
            []
    in
        node tag attrs children



---- SUBS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing then
        Time.every Time.second ChangeImage
    else
        Sub.none



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
