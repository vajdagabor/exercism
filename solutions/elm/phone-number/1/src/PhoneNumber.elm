module PhoneNumber exposing (getNumber)

import Regex


getNumber : String -> Maybe String
getNumber phoneNumber =
    let
        numbers =
            String.filter Char.isDigit phoneNumber

        phonePtn =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^(\\+?1)?[2-9]\\d{2}[2-9]\\d{6}$"
    in
    if Regex.contains phonePtn numbers then
        Just (String.right 10 numbers)

    else
        Nothing
