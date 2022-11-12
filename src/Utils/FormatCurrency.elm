module Utils.FormatCurrency exposing (..)


formatCurrency : Int -> String
formatCurrency amount =
    let
        amountStr =
            String.fromInt amount
    in
    {- Amounts below 10 000 should always be written together -}
    if amount < 10000 then
        amountStr

    else if amount > 999999999999999999 then
        "∞"

    else
        amountStr
            |> String.foldr
                (\char ->
                    \acc ->
                        case acc of
                            current :: rest ->
                                let
                                    charStr =
                                        String.fromChar char
                                in
                                if String.length current < 3 then
                                    (charStr ++ current) :: rest

                                else
                                    charStr :: current :: rest

                            [] ->
                                acc
                )
                [ "" ]
            |> String.join " "
