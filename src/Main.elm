module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Event
import Process exposing (sleep)
import Task
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query
import Utils.FormatCurrency exposing (formatCurrency)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                { title = "Ränta på ränta"
                , body = view model
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


shareUrl : Settings -> String
shareUrl { interest, monthlySavings, start, years } =
    Url.Builder.absolute []
        [ Url.Builder.string interestQueryParam (String.fromFloat interest)
        , Url.Builder.int montlySavingsQueryParam monthlySavings
        , Url.Builder.int startingSavingsQueryParam start
        , Url.Builder.int yearsQueryParam years
        ]


type alias Settings =
    { interest : Float
    , monthlySavings : Int
    , start : Int
    , years : Int
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , settings : Settings
    }


defaultSettings : Settings
defaultSettings =
    Settings
        7.0
        1000
        10000
        20


getSettingsFromQuery : Url.Url -> Settings
getSettingsFromQuery url =
    let
        maybeSettings =
            Url.Parser.parse parseSharedUrl url
    in
    case maybeSettings of
        Just settings ->
            Settings
                (settings.interest |> Maybe.andThen String.toFloat |> Maybe.withDefault 7.0)
                (Maybe.withDefault 1000 settings.monthlySavings)
                (Maybe.withDefault 10000 settings.start)
                (Maybe.withDefault 20 settings.years)

        Nothing ->
            defaultSettings


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { url = url
      , key = key
      , settings = getSettingsFromQuery url
      }
    , Cmd.none
    )


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
      -- Sliders
    | UpdateInterest String
    | UpdateMonthlySavings String
    | UpdateStartbelopp String
    | UpdateYears String
      -- Action buttons
    | Share
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noUpdate : ( Model, Cmd Msg )
        noUpdate =
            ( model, Cmd.none )

        settings =
            model.settings
    in
    case msg of
        UpdateInterest interest ->
            case String.toFloat interest of
                Just i ->
                    let
                        updatedSettings =
                            { settings | interest = i }
                    in
                    ( { model | settings = updatedSettings }, Cmd.none )

                Nothing ->
                    noUpdate

        UpdateMonthlySavings savings ->
            case String.toInt savings of
                Just s ->
                    let
                        updatedSettings =
                            { settings | monthlySavings = s }
                    in
                    ( { model | settings = updatedSettings }, Cmd.none )

                Nothing ->
                    noUpdate

        UpdateStartbelopp starting ->
            case String.toInt starting of
                Just s ->
                    let
                        updatedSettings =
                            { settings | start = s }
                    in
                    ( { model | settings = updatedSettings }, Cmd.none )

                Nothing ->
                    noUpdate

        UpdateYears years ->
            case String.toInt years of
                Just y ->
                    let
                        updatedSettings =
                            { settings | years = y }
                    in
                    ( { model | settings = updatedSettings }, Cmd.none )

                Nothing ->
                    noUpdate

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.replaceUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, settings = getSettingsFromQuery url }
            , Cmd.none
            )

        {- TODO: Copy link to clipboard on click -}
        Share ->
            ( model, Nav.replaceUrl model.key (shareUrl model.settings) )

        Reset ->
            ( model, Nav.replaceUrl model.key (shareUrl defaultSettings) )


view : Model -> List (Html Msg)
view { settings } =
    [ h1 [] [ text "Ränta på ränta" ]
    , form []
        [ div []
            [ label [ Attr.for "interest" ] [ text "Ränta per år (%)" ]
            , input
                [ Attr.type_ "range"
                , Attr.min "0"
                , Attr.max "25"
                , Attr.step "0.1"
                , Attr.value <| String.fromFloat <| settings.interest
                , Event.onInput UpdateInterest
                ]
                []
            , span [] [ text (String.fromFloat settings.interest) ]
            ]
        , div []
            [ label [ Attr.for "monthly-savings" ] [ text "Månadssparande" ]
            , input
                [ Attr.type_ "range"
                , Attr.min "0"
                , Attr.max "20000"
                , Attr.step "100"
                , Attr.value <| String.fromInt <| settings.monthlySavings
                , Event.onInput UpdateMonthlySavings
                ]
                []
            , span [] [ text (formatCurrency settings.monthlySavings) ]
            ]
        , div []
            [ label [ Attr.for "starting" ] [ text "Startbelopp" ]
            , input
                [ Attr.type_ "range"
                , Attr.min "0"
                , Attr.max "1000000"
                , Attr.step "1000"
                , Attr.value <| String.fromInt <| settings.start
                , Event.onInput UpdateStartbelopp
                ]
                []
            , span [] [ text (formatCurrency settings.start) ]
            ]
        , div []
            [ label [ Attr.for "years" ] [ text "Antal år" ]
            , input
                [ Attr.type_ "range"
                , Attr.min "0"
                , Attr.max "40"
                , Attr.step "1"
                , Attr.value <| String.fromInt <| settings.years
                , Event.onInput UpdateYears
                ]
                []
            , span [] [ text (String.fromInt settings.years) ]
            ]
        ]
    , div [ Attr.id "actions" ]
        [ button
            [ Event.onClick Share ]
            [ text "Dela!" ]
        , button
            [ Event.onClick Reset ]
            [ text "Återställ!" ]
        ]
    , div [ Attr.id "results" ]
        [ table []
            [ thead []
                [ tr []
                    (List.map
                        (\c -> th [] [ text c ])
                        [ "År"
                        , "Startvärde"
                        , "Årets sparande"
                        , "Avkastning (kr)"
                        , "Värde vid årets slut"
                        , "Årets sparande (ack.)"
                        , "Avkastning (ack.)"
                        ]
                    )
                ]
            , tbody []
                (List.map
                    mapRow
                    (calculate settings)
                )
            ]
        ]
    ]


mapRow : Row -> Html msg
mapRow { year, start, yearlySavings, yield, valueAtYearsEnd, yearlySavingsAccumulated, yieldAccumulated } =
    tr []
        (List.map
            (\v -> td [] [ text v ])
            [ String.fromInt year
            , formatCurrency (round start)
            , formatCurrency (round yearlySavings)
            , formatCurrency (round yield)
            , formatCurrency (round valueAtYearsEnd)
            , formatCurrency (round yearlySavingsAccumulated)
            , formatCurrency (round yieldAccumulated)
            ]
        )


type alias Row =
    { year : Int
    , start : Float
    , yearlySavings : Float
    , yield : Float
    , valueAtYearsEnd : Float
    , yearlySavingsAccumulated : Float
    , yieldAccumulated : Float
    }


calculate : Settings -> List Row
calculate { monthlySavings, start, interest, years } =
    let
        initalYearlySavings : Int
        initalYearlySavings =
            monthlySavings * 12

        initalYield : Float
        initalYield =
            toFloat (start + initalYearlySavings) * (interest / 100)

        inital : Row
        inital =
            { year = 1
            , start = toFloat start
            , yearlySavings = toFloat initalYearlySavings
            , yield = initalYield
            , valueAtYearsEnd = toFloat start + toFloat initalYearlySavings + initalYield
            , yearlySavingsAccumulated = toFloat initalYearlySavings
            , yieldAccumulated = initalYield
            }
    in
    List.foldl
        (\i ->
            \acc ->
                case acc of
                    previous :: _ ->
                        let
                            yearlySavings =
                                toFloat monthlySavings * 12

                            yield =
                                (previous.valueAtYearsEnd + yearlySavings) * (interest / 100.0)

                            current : Row
                            current =
                                { year = i
                                , start = previous.valueAtYearsEnd
                                , yearlySavings = yearlySavings
                                , yield = yield
                                , valueAtYearsEnd = previous.valueAtYearsEnd + yearlySavings + yield
                                , yearlySavingsAccumulated = previous.yearlySavingsAccumulated + yearlySavings
                                , yieldAccumulated = previous.yieldAccumulated + yield
                                }
                        in
                        current :: acc

                    {- We should always have at least one element, so this can never happen -}
                    [] ->
                        acc
        )
        [ inital ]
        (List.range 2 years)
        |> List.reverse
