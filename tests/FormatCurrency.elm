module FormatCurrency exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Utils.FormatCurrency exposing (formatCurrency)


cases : List ( Int, String )
cases =
    [ ( 100, "100" )
    , ( 1000, "1000" )
    , ( 10000, "10 000" )
    , ( 100000, "100 000" )
    , ( 1000000, "1 000 000" )
    , ( 10000000, "10 000 000" )
    , ( 100000000, "100 000 000" )
    , ( 1000000000, "1 000 000 000" )
    , ( 10000000000, "10 000 000 000" )
    ]


suite : Test
suite =
    describe "It formats an int as currency according to the swedish standard"
        (List.map
            (\( input, wanted ) ->
                test (String.fromInt input ++ " equals " ++ "\"" ++ wanted ++ "\"") <|
                    \_ ->
                        input
                            |> formatCurrency
                            |> Expect.equal wanted
            )
            cases
        )
