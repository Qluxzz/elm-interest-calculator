module Utils.FormatCurrency exposing (..)


formatCurrency : Int -> String
formatCurrency v =
    let
        s =
            String.fromInt v
    in
    {- Amounts below 10 000 should always be written together -}
    if v < 10000 then
        s

    else
        s
            |> String.foldr
                (\c ->
                    \acc ->
                        case acc of
                            current :: rest ->
                                let
                                    cs =
                                        String.fromChar c
                                in
                                if String.length current < 3 then
                                    (cs ++ current) :: rest

                                else
                                    cs :: current :: rest

                            [] ->
                                acc
                )
                [ "" ]
            |> String.join " "
