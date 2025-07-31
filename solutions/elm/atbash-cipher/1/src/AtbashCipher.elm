module AtbashCipher exposing (decode, encode)


encode : String -> String
encode plain =
    String.filter Char.isAlphaNum plain
        |> String.toLower
        |> String.map replaceChar
        |> chunkBy 5


decode : String -> String
decode cipher =
    String.filter Char.isAlphaNum cipher
        |> String.map replaceChar


replaceChar : Char -> Char
replaceChar char =
    case char of
        'a' ->
            'z'

        'b' ->
            'y'

        'c' ->
            'x'

        'd' ->
            'w'

        'e' ->
            'v'

        'f' ->
            'u'

        'g' ->
            't'

        'h' ->
            's'

        'i' ->
            'r'

        'j' ->
            'q'

        'k' ->
            'p'

        'l' ->
            'o'

        'm' ->
            'n'

        'n' ->
            'm'

        'o' ->
            'l'

        'p' ->
            'k'

        'q' ->
            'j'

        'r' ->
            'i'

        's' ->
            'h'

        't' ->
            'g'

        'u' ->
            'f'

        'v' ->
            'e'

        'w' ->
            'd'

        'x' ->
            'c'

        'y' ->
            'b'

        'z' ->
            'a'

        _ ->
            char


chunkBy : Int -> String -> String
chunkBy n str =
    let
        chunk : Char -> String -> String
        chunk char acc =
            let
                charStr =
                    String.fromChar char

                spaceNeeded =
                    modBy (n + 1) (String.length acc + 1) == 0

                space =
                    if spaceNeeded then
                        " "

                    else
                        ""
            in
            acc ++ space ++ charStr
    in
    String.foldl chunk "" str
