module Triangle exposing (Triangle(..), triangleKind)


type Triangle
    = Equilateral
    | Isosceles
    | Scalene


type TriangleError
    = InvalidLengths
    | ViolatesInequality


type alias Sides a =
    ( a, a, a )


triangleKind : number -> number -> number -> Result String Triangle
triangleKind x y z =
    case parseTriangle x y z of
        Ok ( a, b, c ) ->
            if a == b && b == c then
                Ok Equilateral

            else if a == b || b == c || a == c then
                Ok Isosceles

            else
                Ok Scalene

        Err InvalidLengths ->
            Err "Invalid lengths"

        Err ViolatesInequality ->
            Err "Violates inequality"


parseTriangle : number -> number -> number -> Result TriangleError (Sides number)
parseTriangle x y z =
    if x <= 0 || y <= 0 || z <= 0 then
        Err InvalidLengths

    else if x + y <= z || x + z <= y || y + z <= x then
        Err ViolatesInequality

    else
        Ok ( x, y, z )
