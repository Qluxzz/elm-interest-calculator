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
    , interest : Maybe Int
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
            (Url.Parser.Query.int interestQueryParam)
            (Url.Parser.Query.int startingSavingsQueryParam)
        )


shareUrl : Model -> String
shareUrl model =
    Url.Builder.absolute []
        [ Url.Builder.int interestQueryParam model.yearlyInterest
        , Url.Builder.int montlySavingsQueryParam model.monthlySavings
        , Url.Builder.int startingSavingsQueryParam model.start
        , Url.Builder.int yearsQueryParam model.years
        ]


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , yearlyInterest : Int
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
                (Maybe.withDefault 7 settings.interest)
                (Maybe.withDefault 1000 settings.monthlySavings)
                (Maybe.withDefault 10000 settings.start)
                (Maybe.withDefault 20 settings.years)

        Nothing ->
            default key url


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( getSettingsFromQuery key url, Cmd.none )


type Msg
    = UpdateYearlyInterest Int
    | UpdateMonthlySavings Int
    | UpdateStartbelopp Int
    | UpdateYears Int
    | Share
    | Reset
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateYearlyInterest interest ->
            ( { model | yearlyInterest = interest }, Cmd.none )

        UpdateMonthlySavings savings ->
            ( { model | monthlySavings = savings }, Cmd.none )

        UpdateStartbelopp starting ->
            ( { model | start = starting }, Cmd.none )

        UpdateYears years ->
            ( { model | years = years }, Cmd.none )

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


defaultToZero : String -> Int
defaultToZero =
    String.toInt >> Maybe.withDefault 0


intValue : Int -> Attribute msg
intValue =
    String.fromInt >> Attr.value


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
                    , intValue model.yearlyInterest
                    , Event.onInput (\v -> UpdateYearlyInterest (defaultToZero v))
                    ]
                    []
                , span [] [ text (String.fromInt model.yearlyInterest) ]
                ]
            , div []
                [ label [ Attr.for "monthly-savings" ] [ text "Månadssparande" ]
                , input
                    [ Attr.type_ "range"
                    , Attr.min "0"
                    , Attr.max "20000"
                    , Attr.step "100"
                    , intValue model.monthlySavings
                    , Event.onInput (\v -> UpdateMonthlySavings (defaultToZero v))
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
                    , intValue model.start
                    , Event.onInput (\v -> UpdateStartbelopp (defaultToZero v))
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
                    , intValue model.years
                    , Event.onInput (\v -> UpdateYears (defaultToZero v))
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
