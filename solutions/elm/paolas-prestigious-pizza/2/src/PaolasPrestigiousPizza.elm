module PaolasPrestigiousPizza exposing
    ( Pizza
    , ingredientsParser
    , menuParser
    , oneIngredientParser
    , pizzaParser
    , priceParser
    , vegetarianParser
    , wordParser
    )

import Parser exposing ((|.), (|=), Parser)


type alias Pizza =
    { name : String
    , vegetarian : Bool
    , ingredients : List String
    , price : Int
    }


priceParser : Parser Int
priceParser =
    Parser.succeed identity
        |= Parser.int
        |. Parser.symbol "€"


vegetarianParser : Parser Bool
vegetarianParser =
    Parser.oneOf
        [ Parser.succeed True |. Parser.keyword "(v)"
        , Parser.succeed False
        ]


wordParser : Parser String
wordParser =
    Parser.succeed String.toLower
        |= (Parser.chompWhile Char.isAlpha |> Parser.getChompedString)


ingredientsParser : Parser (List String)
ingredientsParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ","
        , spaces = Parser.spaces
        , item = oneIngredientParser
        , trailing = Parser.Forbidden
        }


pizzaParser : Parser Pizza
pizzaParser =
    Parser.succeed Pizza
        |= oneIngredientParser
        |. Parser.spaces
        |= vegetarianParser
        |. Parser.symbol ":"
        |. Parser.spaces
        |= ingredientsParser
        |. Parser.spaces
        |. Parser.symbol "-"
        |. Parser.spaces
        |= priceParser


menuParser : Parser (List Pizza)
menuParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = "\n"
        , spaces = Parser.chompWhile ((==) ' ')
        , item = pizzaParser
        , trailing = Parser.Optional
        }
        |. Parser.end


oneIngredientParser : Parser String
oneIngredientParser =
    Parser.succeed (String.toLower >> String.trim)
        |= (Parser.chompWhile isAlphaOrSpace |> Parser.getChompedString)
        |> Parser.andThen checkIfEmpty


isAlphaOrSpace : Char -> Bool
isAlphaOrSpace c =
    Char.isAlpha c || c == ' '


checkIfEmpty : String -> Parser String
checkIfEmpty s =
    case s of
        "" ->
            Parser.problem "empty string"

        _ ->
            Parser.succeed s
