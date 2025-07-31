module AtbashCipher exposing (decode, encode)


encode : String -> String
encode plain =
    String.filter Char.isAlphaNum plain
        |> String.toLower
        |> String.map swap
        |> chunkBy 5
        |> String.join " "


decode : String -> String
decode cipher =
    String.filter Char.isAlphaNum cipher
        |> String.map swap


swap : Char -> Char
swap char =
    if Char.isAlpha char then
        Char.toCode 'z' - (Char.toCode char - Char.toCode 'a') |> Char.fromCode

    else
        char


chunkBy : Int -> String -> List String
chunkBy n str =
    if String.isEmpty str then
        []

    else
        String.left n str :: chunkBy n (String.dropLeft n str)
