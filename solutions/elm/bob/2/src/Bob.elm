module Bob exposing (hey)


hey : String -> String
hey remark =
    if isYelling remark && isQuestion remark then
        "Calm down, I know what I'm doing!"

    else if isQuestion remark then
        "Sure."

    else if isYelling remark then
        "Whoa, chill out!"

    else if isSilence remark then
        "Fine. Be that way!"

    else
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
