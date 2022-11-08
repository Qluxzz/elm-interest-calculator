module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Event
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                { title = "Ränta på ränta"
                , body = [ view model |> toUnstyled ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Query =
    { monthlySavings : Maybe Int
    , years : Maybe Int
    , interest : Maybe String
    , start : Maybe Int
    }


yearsQueryParam : String
yearsQueryParam =
    "years"


interestQueryParam : String
interestQueryParam =
    "interest"


montlySavingsQueryParam : String
montlySavingsQueryParam =
    "monthlySavings"


startingSavingsQueryParam : String
startingSavingsQueryParam =
    "start"


parseSharedUrl : Url.Parser.Parser (Query -> a) a
parseSharedUrl =
    Url.Parser.query
        (Url.Parser.Query.map4
            Query
            (Url.Parser.Query.int montlySavingsQueryParam)
            (Url.Parser.Query.int yearsQueryParam)
            (Url.Parser.Query.string interestQueryParam)
            (Url.Parser.Query.int startingSavingsQueryParam)
        )


shareUrl : Model -> String
shareUrl model =
    Url.Builder.absolute []
        [ Url.Builder.string interestQueryParam (String.fromFloat model.interest)
        , Url.Builder.int montlySavingsQueryParam model.monthlySavings
        , Url.Builder.int startingSavingsQueryParam model.start
        , Url.Builder.int yearsQueryParam model.years
        ]


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , interest : Float
    , monthlySavings : Int
    , start : Int
    , years : Int
    }


default : Nav.Key -> Url.Url -> Model
default key url =
    Model
        key
        url
        7
        1000
        10000
        20


getSettingsFromQuery : Nav.Key -> Url.Url -> Model
getSettingsFromQuery key url =
    let
        maybeSettings =
            Url.Parser.parse parseSharedUrl url
    in
    case maybeSettings of
        Just settings ->
            Model
                key
                url
                (settings.interest |> Maybe.andThen String.toFloat |> Maybe.withDefault 7.0)
                (Maybe.withDefault 1000 settings.monthlySavings)
                (Maybe.withDefault 10000 settings.start)
                (Maybe.withDefault 20 settings.years)

        Nothing ->
            default key url


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( getSettingsFromQuery key url, Cmd.none )


type Msg
    = UpdateInterest String
    | UpdateMonthlySavings String
    | UpdateStartbelopp String
    | UpdateYears String
    | Share
    | Reset
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noUpdate : ( Model, Cmd Msg )
        noUpdate =
            ( model, Cmd.none )
    in
    case msg of
        UpdateInterest interest ->
            case String.toFloat interest of
                Just i ->
                    ( { model | interest = i }, Cmd.none )

                Nothing ->
                    noUpdate

        UpdateMonthlySavings savings ->
            case String.toInt savings of
                Just s ->
                    ( { model | monthlySavings = s }, Cmd.none )

                Nothing ->
                    noUpdate

        UpdateStartbelopp starting ->
            case String.toInt starting of
                Just s ->
                    ( { model | start = s }, Cmd.none )

                Nothing ->
                    noUpdate

        UpdateYears years ->
            case String.toInt years of
                Just y ->
                    ( { model | years = y }, Cmd.none )

                Nothing ->
                    noUpdate

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.replaceUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( getSettingsFromQuery model.key url
            , Cmd.none
            )

        {- TODO: Copy link to clipboard on click -}
        Share ->
            ( model, Nav.replaceUrl model.key (shareUrl model) )

        Reset ->
            let
                defaultModel =
                    default model.key model.url
            in
            ( defaultModel, Nav.replaceUrl model.key (shareUrl defaultModel) )


view : Model -> Html.Styled.Html Msg
view model =
    div []
        [ h1 [] [ text "Ränta på ränta" ]
        , form []
            [ div []
                [ label [ Attr.for "interest" ] [ text "Ränta per år (%)" ]
                , input
                    [ Attr.type_ "range"
                    , Attr.min "0"
                    , Attr.max "25"
                    , Attr.step "0.1"
                    , Attr.value <| String.fromFloat <| model.interest
                    , Event.onInput UpdateInterest
                    ]
                    []
                , span [] [ text (String.fromFloat model.interest) ]
                ]
            , div []
                [ label [ Attr.for "monthly-savings" ] [ text "Månadssparande" ]
                , input
                    [ Attr.type_ "range"
                    , Attr.min "0"
                    , Attr.max "20000"
                    , Attr.step "100"
                    , Attr.value <| String.fromInt <| model.monthlySavings
                    , Event.onInput UpdateMonthlySavings
                    ]
                    []
                , span [] [ text (String.fromInt model.monthlySavings) ]
                ]
            , div []
                [ label [ Attr.for "starting" ] [ text "Startbelopp" ]
                , input
                    [ Attr.type_ "range"
                    , Attr.min "0"
                    , Attr.max "1000000"
                    , Attr.step "1000"
                    , Attr.value <| String.fromInt <| model.start
                    , Event.onInput UpdateStartbelopp
                    ]
                    []
                , span [] [ text (String.fromInt model.start) ]
                ]
            , div []
                [ label [ Attr.for "years" ] [ text "Antal år" ]
                , input
                    [ Attr.type_ "range"
                    , Attr.min "0"
                    , Attr.max "20"
                    , Attr.step "1"
                    , Attr.value <| String.fromInt <| model.years
                    , Event.onInput UpdateYears
                    ]
                    []
                , span [] [ text (String.fromInt model.years) ]
                ]
            ]
        , div []
            [ button
                [ Event.onClick Share ]
                [ text "Dela!" ]
            , button
                [ Event.onClick Reset ]
                [ text "Återställ!" ]
            ]
        ]
