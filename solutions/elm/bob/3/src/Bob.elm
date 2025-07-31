module Bob exposing (hey)


type RemarkType
    = Question
    | Yelling
    | YellingQuestion
    | Silence
    | Default


hey : String -> String
hey =
    toRemarkType >> toReply


toRemarkType : String -> RemarkType
toRemarkType remark =
    if isQuestion remark && isYelling remark then
        YellingQuestion

    else if isQuestion remark then
        Question

    else if isYelling remark then
        Yelling

    else if isSilence remark then
        Silence

    else
        Default


toReply : RemarkType -> String
toReply remarkType =
    case remarkType of
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


isQuestion : String -> Bool
isQuestion =
    String.trim >> String.endsWith "?"


isYelling : String -> Bool
isYelling remark =
    String.toUpper remark == remark && String.toLower remark /= remark


isSilence : String -> Bool
isSilence =
    String.trim >> String.isEmpty
