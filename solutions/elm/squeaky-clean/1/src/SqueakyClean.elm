module SqueakyClean exposing (clean, clean1, clean2, clean3, clean4)

-- Spaces to underscores


clean1 : String -> String
clean1 =
    String.replace " " "_"



-- Replace control chars


clean2 : String -> String
clean2 str =
    let
        replaceCtrl symbol =
            String.replace symbol "[CTRL]"
    in
    str
        |> clean1
        |> replaceCtrl "\n"
        |> replaceCtrl "\t"
        |> replaceCtrl "\u{000D}"



-- Kebab to camel case


clean3 : String -> String
clean3 str =
    let
        capitalizeIfNotFirst : Int -> String -> String
        capitalizeIfNotFirst index word =
            if index == 0 then
                word

            else
                capitalize word
    in
    str
        |> clean2
        |> String.split "-"
        |> List.indexedMap capitalizeIfNotFirst
        |> String.join ""



-- Omit digits


clean4 : String -> String
clean4 str =
    str
        |> clean3
        |> String.filter (\c -> not (Char.isDigit c))



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
omitGreekLetters str =
    String.filter (\c -> not (isGreekLetter c)) str


isGreekLetter : Char -> Bool
isGreekLetter char =
    let
        code =
            Char.toCode char

        alpha =
            945

        omega =
            969
    in
    code >= alpha && code <= omega
