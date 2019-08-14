module Order exposing (..)


orderStrings : String -> String -> ( String, String )
orderStrings str1 str2 =
    let
        result1 =
            String.uncons str1

        result2 =
            String.uncons str2
    in
    case ( result1, result2 ) of
        ( Just ( chr1, rest1 ), Just ( chr2, rest2 ) ) ->
            if chr1 == chr2 then
                String.cons chr1 (orderStrings rest1 rest2)

            else
                ( str1, str2 )

        ( _, _ ) ->
            ( str1, str2 )


inBetween : String -> String -> String
inBetween str1 str2 =
    let
        result1 =
            String.uncons str1

        result2 =
            String.uncons str2
    in
    case ( result1, result2 ) of
        ( Just ( chr1, rest1 ), Just ( chr2, rest2 ) ) ->
            if chr1 == chr2 then
                String.cons chr1 (inBetween rest1 rest2)

            else
                let
                    charDiff =
                        Char.toCode chr2 - Char.toCode chr1
                in
                if abs charDiff == 1 then
                    String.cons (Char.fromCode (Char.toCode chr1 + 1)) "m"

                else
                    Char.fromCode (Char.toCode chr1 + (charDiff / 2))

        ( _, _ ) ->
            "m"
