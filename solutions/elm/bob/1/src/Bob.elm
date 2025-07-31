module Bob exposing (hey)


type Tone
    = Question
    | Yelling
    | YellingQuestion
    | Default
    | Silence


hey : String -> String
hey remark =
    case toneOf remark of
        YellingQuestion ->
            "Calm down, I know what I'm doing!"

        Question ->
            "Sure."

        Yelling ->
            "Whoa, chill out!"

        Silence ->
            "Fine. Be that way!"

        Default ->
            "Whatever."


toneOf : String -> Tone
toneOf remark =
    case ( isYelling remark, isQuestion remark, isSilence remark ) of
        ( True, True, _ ) ->
            YellingQuestion

        ( _, True, _ ) ->
            Question

        ( True, _, _ ) ->
            Yelling

        ( _, _, True ) ->
            Silence

        _ ->
            Default


isQuestion : String -> Bool
isQuestion =
    String.trim >> String.endsWith "?"


isYelling : String -> Bool
isYelling remark =
    let
        letters =
            String.filter Char.isAlpha remark
    in
    (not <| String.isEmpty letters)
        && String.all Char.isUpper letters


isSilence : String -> Bool
isSilence =
    String.trim >> String.isEmpty
