module Utils.FormatCurrency exposing (..)

import Dict


formatCurrency : Int -> String
formatCurrency v =
    case v |> String.fromInt |> String.split "" of
        a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: [] ->
            a ++ " " ++ b ++ c ++ d ++ " " ++ e ++ f ++ g ++ " " ++ h ++ i ++ j

        a :: b :: c :: d :: e :: f :: g :: h :: i :: [] ->
            a ++ b ++ c ++ " " ++ d ++ e ++ f ++ " " ++ g ++ h ++ i

        a :: b :: c :: d :: e :: f :: g :: h :: [] ->
            a ++ b ++ " " ++ c ++ d ++ e ++ " " ++ f ++ g ++ h

        a :: b :: c :: d :: e :: f :: g :: [] ->
            a ++ " " ++ b ++ c ++ d ++ " " ++ e ++ f ++ g

        a :: b :: c :: d :: e :: f :: [] ->
            a ++ b ++ c ++ " " ++ d ++ e ++ f

        a :: b :: c :: d :: e :: [] ->
            a ++ b ++ " " ++ c ++ d ++ e

        a :: b :: c :: d :: [] ->
            a ++ b ++ c ++ d

        x :: y :: z :: [] ->
            x ++ y ++ z

        x :: y :: [] ->
            x ++ y

        x :: [] ->
            x

        [] ->
            "0"

        _ ->
            "not implemented"


holes =
    Dict.fromList
        [ ( 5, [ 1 ] ) -- 10 000
        , ( 6, [ 2 ] ) -- 100 000
        , ( 7, [ 0, 3 ] ) -- 1 000 000
        , ( 8, [ 1, 4 ] ) -- 10 000 000
        , ( 9, [ 2, 5 ] ) -- 100 000 000
        , ( 10, [ 0, 3, 6 ] ) -- 1 000 000 000
        ]


formatCurrency2 : Int -> String
formatCurrency2 v =
    let
        s =
            String.fromInt v

        length =
            String.length s
    in
    s
        |> String.split ""
        |> List.indexedMap Tuple.pair
        |> List.foldr
            (\( i, ss ) ->
                \acc ->
                    case Dict.get length holes of
                        Just h ->
                            if List.member i h then
                                ss ++ " " ++ acc

                            else
                                ss ++ acc

                        Nothing ->
                            ss ++ acc
            )
            ""
