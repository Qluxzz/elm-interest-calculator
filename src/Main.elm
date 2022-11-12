port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Event
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


type Field
    = Years
    | Interest
    | MonthlySaving
    | Start


toQueryParam : Field -> String
toQueryParam f =
    case f of
        Years ->
            "years"

        Interest ->
            "interest"

        MonthlySaving ->
            "monthlySavings"

        Start ->
            "start"


parseSharedUrl : Url.Parser.Parser (Query -> a) a
parseSharedUrl =
    Url.Parser.query
        (Url.Parser.Query.map4
            Query
            (Url.Parser.Query.int (toQueryParam MonthlySaving))
            (Url.Parser.Query.int (toQueryParam Years))
            (Url.Parser.Query.string (toQueryParam Interest))
            (Url.Parser.Query.int (toQueryParam Start))
        )


shareUrl : Settings -> String
shareUrl { interest, monthlySavings, start, years } =
    Url.Builder.toQuery
        [ Url.Builder.string (toQueryParam Interest) (String.fromFloat interest)
        , Url.Builder.int (toQueryParam MonthlySaving) monthlySavings
        , Url.Builder.int (toQueryParam Start) start
        , Url.Builder.int (toQueryParam Years) years
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
    , currentlyFocused : Maybe ( Field, String )
    }


defaultSettings : Settings
defaultSettings =
    Settings
        7.0
        1000
        10000
        20


getSettingsFromQuery : Url.Url -> Maybe Settings
getSettingsFromQuery url =
    let
        maybeSettings =
            {- Clear path so parser run regardless of which path it is on -}
            { url | path = "" }
                |> Url.Parser.parse parseSharedUrl
    in
    Maybe.andThen
        (\settings ->
            Maybe.map4
                (\interest ->
                    \monthlySavings ->
                        \start ->
                            \years ->
                                Settings
                                    interest
                                    monthlySavings
                                    start
                                    years
                )
                (settings.interest |> Maybe.andThen String.toFloat)
                settings.monthlySavings
                settings.start
                settings.years
        )
        maybeSettings


getSettingsFromQueryOrDefault : Url.Url -> Settings
getSettingsFromQueryOrDefault =
    getSettingsFromQuery >> Maybe.withDefault defaultSettings


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { url = url
      , key = key
      , settings = getSettingsFromQueryOrDefault url
      , currentlyFocused = Nothing
      }
    , Cmd.none
    )


port shareSettings : String -> Cmd msg


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
      -- Sliders
    | UpdateInterest String
    | UpdateMonthlySavings String
    | UpdateStartbelopp String
    | UpdateYears String
      -- Text input
    | FocusField Field
    | ApplyDraft
    | UpdateDraft String
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
            case starting |> String.toInt |> Maybe.map (\s -> { settings | start = s }) of
                Just updated ->
                    ( { model | settings = updated }, Cmd.none )

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
            ( { model | url = url, settings = getSettingsFromQueryOrDefault url }
            , Cmd.none
            )

        Share ->
            let
                queryParams =
                    shareUrl model.settings
            in
            ( model
            , Cmd.batch
                [ Nav.replaceUrl model.key queryParams
                , shareSettings queryParams
                ]
            )

        Reset ->
            ( { model | settings = getSettingsFromQueryOrDefault model.url }, Cmd.none )

        FocusField field ->
            let
                valueStr =
                    case field of
                        Interest ->
                            String.fromFloat settings.interest

                        MonthlySaving ->
                            String.fromInt settings.monthlySavings

                        Years ->
                            String.fromInt settings.years

                        Start ->
                            String.fromInt settings.start
            in
            ( { model | currentlyFocused = Just ( field, valueStr ) }, Cmd.none )

        ApplyDraft ->
            let
                updatedSettings : Settings
                updatedSettings =
                    case model.currentlyFocused of
                        Just ( MonthlySaving, draft ) ->
                            { settings | monthlySavings = draft |> String.toInt |> Maybe.withDefault settings.monthlySavings }

                        Just ( Interest, draft ) ->
                            { settings
                                | interest =
                                    draft
                                        -- Replace decimal seperator with valid for String.toFloat
                                        |> String.replace "," "."
                                        |> String.toFloat
                                        |> Maybe.withDefault settings.interest
                            }

                        Just ( Start, draft ) ->
                            { settings | start = draft |> String.toInt |> Maybe.withDefault settings.start }

                        Just ( Years, draft ) ->
                            { settings | years = draft |> String.toInt |> Maybe.withDefault settings.years }

                        _ ->
                            settings
            in
            ( { model
                | currentlyFocused = Nothing
                , settings = updatedSettings
              }
            , Cmd.none
            )

        UpdateDraft str ->
            let
                updated =
                    model.currentlyFocused |> Maybe.map (Tuple.mapSecond (\_ -> str))
            in
            ( { model | currentlyFocused = updated }, Cmd.none )


numericTextInput : List (Attribute msg) -> List (Html msg) -> Html msg
numericTextInput attr =
    input
        ([ Attr.type_ "text"
         , Attr.pattern "[0-9]*"
         , Attr.attribute "inputmode" "numeric"
         ]
            ++ attr
        )


decimalTextInput : List (Attribute msg) -> List (Html msg) -> Html msg
decimalTextInput attr =
    numericTextInput
        ([ Attr.attribute "inputmode" "decimal"
         ]
            ++ attr
        )


view : Model -> List (Html Msg)
view { settings, currentlyFocused } =
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
            , decimalTextInput
                [ Attr.type_ "text"
                , Attr.value
                    (case currentlyFocused of
                        Just ( Interest, draft ) ->
                            draft

                        _ ->
                            settings.interest |> String.fromFloat
                    )
                , Event.onFocus (FocusField Interest)
                , Event.onInput UpdateDraft
                , Event.onBlur ApplyDraft
                ]
                []
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
            , numericTextInput
                [ Attr.value
                    (case currentlyFocused of
                        Just ( MonthlySaving, draft ) ->
                            draft

                        _ ->
                            formatCurrency settings.monthlySavings
                    )
                , Event.onFocus (FocusField MonthlySaving)
                , Event.onInput UpdateDraft
                , Event.onBlur ApplyDraft
                ]
                []
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
            , numericTextInput
                [ Attr.value
                    (case currentlyFocused of
                        Just ( Start, draft ) ->
                            draft

                        _ ->
                            formatCurrency settings.start
                    )
                , Event.onFocus (FocusField Start)
                , Event.onInput UpdateDraft
                , Event.onBlur ApplyDraft
                ]
                []
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
            , numericTextInput
                [ Attr.value
                    (case currentlyFocused of
                        Just ( Years, draft ) ->
                            draft

                        _ ->
                            formatCurrency settings.years
                    )
                , Event.onFocus (FocusField Years)
                , Event.onInput UpdateDraft
                , Event.onBlur ApplyDraft
                ]
                []
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
                        , "Sparande"
                        , "Avkastning"
                        , "Värde vid årets slut"
                        , "Sparande (ack.)"
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
