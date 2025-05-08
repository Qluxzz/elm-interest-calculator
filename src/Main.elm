port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Event
import Process
import String
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
    , savingsIncrease : Maybe String
    }


type Field
    = Years
    | Interest
    | MonthlySaving
    | Start
    | SavingsIncrease


fieldId : Field -> String
fieldId field =
    case field of
        Years ->
            "years"

        Interest ->
            "interest"

        MonthlySaving ->
            "monthly-savings"

        Start ->
            "start"

        SavingsIncrease ->
            "savings-increase"


fieldTitle : Field -> String
fieldTitle field =
    case field of
        Interest ->
            "Ränta per år (%)"

        MonthlySaving ->
            "Månadssparande"

        Start ->
            "Startbelopp"

        Years ->
            "Antal år"

        SavingsIncrease ->
            "Ökning av sparande (%)"


fieldUpdate : Field -> (String -> Msg)
fieldUpdate field =
    case field of
        Years ->
            UpdateYears

        Interest ->
            UpdateInterest

        MonthlySaving ->
            UpdateMonthlySavings

        Start ->
            UpdateStartbelopp

        SavingsIncrease ->
            UpdateSavingsIncrease


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

        SavingsIncrease ->
            "increase"


parseSharedUrl : Url.Parser.Parser (Query -> a) a
parseSharedUrl =
    Url.Parser.query
        (Url.Parser.Query.map5
            Query
            (Url.Parser.Query.int (toQueryParam MonthlySaving))
            (Url.Parser.Query.int (toQueryParam Years))
            (Url.Parser.Query.string (toQueryParam Interest))
            (Url.Parser.Query.int (toQueryParam Start))
            (Url.Parser.Query.string (toQueryParam SavingsIncrease))
        )


shareUrl : Settings -> String
shareUrl { interest, monthlySavings, start, years, savingsIncrease } =
    Url.Builder.toQuery
        [ Url.Builder.string (toQueryParam Interest) (String.fromFloat interest)
        , Url.Builder.int (toQueryParam MonthlySaving) monthlySavings
        , Url.Builder.int (toQueryParam Start) start
        , Url.Builder.int (toQueryParam Years) years
        , Url.Builder.string (toQueryParam SavingsIncrease) (String.fromFloat savingsIncrease)
        ]


type alias Percentage =
    Float


type alias Settings =
    { interest : Percentage
    , monthlySavings : Int
    , start : Int
    , years : Int
    , savingsIncrease : Percentage
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , currentSettings : Settings
    , initialSettings : Settings
    , currentlyFocused : Maybe ( Field, String )
    }


defaultSettings : Settings
defaultSettings =
    { interest = 7.0
    , monthlySavings = 1000
    , start = 10000
    , years = 20
    , savingsIncrease = 0.0
    }


getSettingsFromQuery : Url.Url -> Settings
getSettingsFromQuery url =
    let
        maybeSettings =
            {- Clear path so parser run regardless of which path it is on -}
            { url | path = "" }
                |> Url.Parser.parse parseSharedUrl
    in
    case maybeSettings of
        Just settings ->
            Settings
                (settings.interest |> Maybe.andThen String.toFloat |> Maybe.withDefault defaultSettings.interest)
                (settings.monthlySavings |> Maybe.withDefault defaultSettings.monthlySavings)
                (settings.start |> Maybe.withDefault defaultSettings.start)
                (settings.years |> Maybe.withDefault defaultSettings.years)
                (settings.savingsIncrease |> Maybe.andThen String.toFloat |> Maybe.withDefault defaultSettings.savingsIncrease)

        Nothing ->
            defaultSettings


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        settings =
            getSettingsFromQuery url
    in
    ( { url = url
      , key = key
      , currentSettings = settings
      , initialSettings = settings
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
    | UpdateSavingsIncrease String
      -- Text input
    | FocusField Field
    | ApplyDraft
    | UpdateDraft String
      -- Action buttons
    | Share
    | Reset
    | DebounceQueryStringUpdate Settings


{-| Replace decimal separator with valid for String.toFloat
-}
internationalToFloat : String -> Maybe Float
internationalToFloat =
    String.replace "," "." >> String.toFloat


toFloatWithDefault : Float -> String -> Float
toFloatWithDefault default =
    internationalToFloat >> Maybe.withDefault default


debounce : Float -> Msg -> Cmd Msg
debounce ms msg =
    Process.sleep ms
        |> Task.perform (\_ -> msg)


debounceQueryStringUpdate : Settings -> Cmd Msg
debounceQueryStringUpdate settings =
    debounce 500 (DebounceQueryStringUpdate settings)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noUpdate : ( Model, Cmd Msg )
        noUpdate =
            ( model, Cmd.none )

        settings =
            model.currentSettings
    in
    case msg of
        UpdateInterest interest ->
            case String.toFloat interest of
                Just i ->
                    let
                        updated =
                            { settings | interest = i }
                    in
                    ( { model | currentSettings = updated }, debounceQueryStringUpdate updated )

                Nothing ->
                    noUpdate

        UpdateMonthlySavings savings ->
            case String.toInt savings of
                Just s ->
                    let
                        updated =
                            { settings | monthlySavings = s }
                    in
                    ( { model | currentSettings = updated }, debounceQueryStringUpdate updated )

                Nothing ->
                    noUpdate

        UpdateStartbelopp starting ->
            case starting |> String.toInt |> Maybe.map (\s -> { settings | start = s }) of
                Just updated ->
                    ( { model | currentSettings = updated }, debounceQueryStringUpdate updated )

                Nothing ->
                    noUpdate

        UpdateYears years ->
            case String.toInt years of
                Just y ->
                    let
                        updated =
                            { settings | years = y }
                    in
                    ( { model | currentSettings = updated }, debounceQueryStringUpdate updated )

                Nothing ->
                    noUpdate

        UpdateSavingsIncrease increase ->
            case String.toFloat increase of
                Just i ->
                    let
                        updated =
                            { settings | savingsIncrease = i }
                    in
                    ( { model | currentSettings = updated }, debounceQueryStringUpdate updated )

                Nothing ->
                    noUpdate

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.replaceUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, currentSettings = getSettingsFromQuery url }
            , Cmd.none
            )

        Share ->
            let
                queryParams =
                    shareUrl model.currentSettings
            in
            ( model
            , Cmd.batch
                [ Nav.replaceUrl model.key queryParams
                , shareSettings queryParams
                ]
            )

        Reset ->
            ( { model | currentSettings = model.initialSettings }, debounceQueryStringUpdate model.initialSettings )

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

                        SavingsIncrease ->
                            String.fromFloat settings.savingsIncrease
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
                                        |> toFloatWithDefault settings.interest
                            }

                        Just ( Start, draft ) ->
                            { settings | start = draft |> String.toInt |> Maybe.withDefault settings.start }

                        Just ( Years, draft ) ->
                            { settings | years = draft |> String.toInt |> Maybe.withDefault settings.years }

                        Just ( SavingsIncrease, draft ) ->
                            { settings
                                | savingsIncrease =
                                    draft
                                        |> toFloatWithDefault settings.savingsIncrease
                            }

                        Nothing ->
                            settings
            in
            ( { model
                | currentlyFocused = Nothing
                , currentSettings = updatedSettings
              }
            , debounceQueryStringUpdate updatedSettings
            )

        UpdateDraft str ->
            let
                updated =
                    model.currentlyFocused |> Maybe.map (Tuple.mapSecond (\_ -> str))
            in
            ( { model | currentlyFocused = updated }, Cmd.none )

        DebounceQueryStringUpdate updatedSettings ->
            if updatedSettings == settings then
                ( model, Nav.replaceUrl model.key (shareUrl settings) )

            else
                ( model, Cmd.none )


numericTextInput : Field -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
numericTextInput field attr =
    input
        ([ Attr.type_ "text"
         , Attr.pattern "[0-9]*"
         , Attr.attribute "inputmode" "numeric"
         , Event.onFocus (FocusField field)
         , Event.onInput UpdateDraft
         , Event.onBlur ApplyDraft
         ]
            ++ attr
        )


decimalTextInput : Field -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
decimalTextInput field attr =
    numericTextInput field
        (Attr.attribute "inputmode" "decimal" :: attr)


markedSlider : Field -> (number -> String) -> List (Attribute Msg) -> number -> number -> List (Html Msg)
markedSlider field toString attrs current initial =
    let
        title =
            fieldTitle field

        id =
            fieldId field

        onChange =
            fieldUpdate field
    in
    [ Html.label [ Attr.for id ] [ text title ]
    , Html.input
        (attrs
            ++ [ Attr.type_ "range"
               , Attr.tabindex -1
               , Attr.value (toString current)
               , Attr.list (id ++ "-marker")
               , Event.onInput onChange
               ]
        )
        []
    , Html.datalist [ Attr.id (id ++ "-marker") ]
        [ Html.option [ Attr.value <| toString initial ] []
        ]
    ]


decimalInputWithSlider : Maybe ( Field, String ) -> Field -> List (Attribute Msg) -> Float -> Float -> List (Html Msg)
decimalInputWithSlider currentlyFocused field attrs current initial =
    markedSlider field String.fromFloat attrs current initial
        ++ [ decimalTextInput
                field
                [ Attr.value
                    (case currentlyFocused of
                        Just ( f, draft ) ->
                            if f == field then
                                draft

                            else
                                String.fromFloat current

                        _ ->
                            String.fromFloat current
                    )
                ]
                []
           ]


numericInputWithSlider : Maybe ( Field, String ) -> Field -> List (Attribute Msg) -> Int -> Int -> List (Html Msg)
numericInputWithSlider currentlyFocused field attrs current initial =
    markedSlider field String.fromInt attrs current initial
        ++ [ numericTextInput
                field
                [ Attr.value
                    (case currentlyFocused of
                        Just ( f, draft ) ->
                            if f == field then
                                draft

                            else
                                String.fromInt current

                        _ ->
                            String.fromInt current
                    )
                ]
                []
           ]


view : Model -> List (Html Msg)
view { initialSettings, currentSettings, currentlyFocused } =
    let
        decimalInput =
            decimalInputWithSlider currentlyFocused

        numericInput =
            numericInputWithSlider currentlyFocused
    in
    [ section []
        [ h1 [] [ text "Ränta på ränta" ]
        , form []
            [ div []
                (decimalInput
                    Interest
                    [ Attr.min "0", Attr.max "25", Attr.step "0.1" ]
                    currentSettings.interest
                    initialSettings.interest
                )
            , div []
                (numericInput
                    MonthlySaving
                    [ Attr.min "0"
                    , Attr.max "20000"
                    , Attr.step "100"
                    ]
                    currentSettings.monthlySavings
                    initialSettings.monthlySavings
                )
            , div []
                (numericInput
                    Start
                    [ Attr.min "0"
                    , Attr.max "1000000"
                    , Attr.step "1000"
                    ]
                    currentSettings.start
                    initialSettings.start
                )
            , div []
                (numericInput
                    Years
                    [ Attr.min "0", Attr.max "40", Attr.step "1" ]
                    currentSettings.years
                    initialSettings.years
                )
            , div []
                (decimalInput
                    SavingsIncrease
                    [ Attr.min "0"
                    , Attr.max "100"
                    , Attr.step "0.1"
                    ]
                    currentSettings.savingsIncrease
                    initialSettings.savingsIncrease
                )
            ]
        , div [ Attr.id "actions" ]
            [ button
                [ Event.onClick Share ]
                [ text "Dela!" ]
            , button
                [ Event.onClick Reset, Attr.disabled (initialSettings == currentSettings) ]
                [ text "Återställ!" ]
            ]
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
                (List.map2
                    mapRow
                    (calculate currentSettings)
                    (calculate { initialSettings | years = currentSettings.years })
                )
            ]
        ]
    ]


mapRow : Row -> Row -> Html msg
mapRow current initial =
    tr []
        (List.map
            (\v -> td [] [ v ])
            [ Html.text <| String.fromInt current.year
            , formatRow current.start initial.start
            , formatRow current.yearlySavings initial.yearlySavings
            , formatRow current.yield initial.yield
            , formatRow current.valueAtYearsEnd initial.valueAtYearsEnd
            , formatRow current.yearlySavingsAccumulated initial.yearlySavingsAccumulated
            , formatRow current.yieldAccumulated initial.yieldAccumulated
            ]
        )


formatRow : Float -> Float -> Html msg
formatRow current initial =
    if current == initial then
        Html.text <| formatCurrency (round current)

    else
        let
            difference =
                current - initial

            sign =
                if difference > 0 then
                    "+"

                else
                    "-"

            color =
                if difference > 0 then
                    "green"

                else
                    "red"
        in
        Html.p []
            [ Html.text <| formatCurrency (round current)
            , Html.text " "
            , Html.span [ Attr.style "color" color ]
                [ Html.text <|
                    "("
                        ++ sign
                        ++ formatCurrency (round (abs difference))
                        ++ ")"
                ]
            ]


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
calculate { monthlySavings, start, interest, years, savingsIncrease } =
    let
        initialYearlySavings : Int
        initialYearlySavings =
            monthlySavings * 12

        initialYield : Float
        initialYield =
            toFloat (start + initialYearlySavings) * (interest / 100)

        initial : Row
        initial =
            { year = 1
            , start = toFloat start
            , yearlySavings = toFloat initialYearlySavings
            , yield = initialYield
            , valueAtYearsEnd = toFloat start + toFloat initialYearlySavings + initialYield
            , yearlySavingsAccumulated = toFloat initialYearlySavings
            , yieldAccumulated = initialYield
            }
    in
    List.foldl
        (\i ->
            \acc ->
                case acc of
                    previous :: _ ->
                        let
                            yearlySavings =
                                previous.yearlySavings + previous.yearlySavings * (savingsIncrease / 100)

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
        [ initial ]
        (List.range 2 years)
        |> List.reverse
