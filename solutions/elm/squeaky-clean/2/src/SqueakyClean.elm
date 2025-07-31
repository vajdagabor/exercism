module SqueakyClean exposing (clean, clean1, clean2, clean3, clean4)

-- Spaces to underscores


clean1 : String -> String
clean1 =
    String.replace " " "_"



-- Replace control chars


clean2 : String -> String
clean2 str =
    let
        ctrlChars =
            [ "\n", "\t", "\u{000D}" ]
    in
    List.foldl (\ctrl -> String.replace ctrl "[CTRL]") (clean1 str) ctrlChars



-- Kebab to camel case


clean3 : String -> String
clean3 str =
    case String.split "-" (clean2 str) of
        [] ->
            ""

        first :: rest ->
            String.concat (first :: List.map capitalize rest)



-- Omit digits


clean4 : String -> String
clean4 =
    clean3 >> String.filter (Char.isDigit >> not)



-- Omit Greek lower case letters


clean : String -> String
clean str =
    str
        |> clean4
        |> omitGreekLetters



-- Helpers


capitalize : String -> String
capitalize word =
    String.toUpper (String.left 1 word) ++ String.dropLeft 1 word


omitGreekLetters : String -> String
omitGreekLetters =
    String.filter (isGreekLetter >> not)


isGreekLetter : Char -> Bool
isGreekLetter char =
    let
        code =
            Char.toCode char
    in
    code >= Char.toCode 'α' && code <= Char.toCode 'ω'
