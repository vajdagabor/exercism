module GradeSchool exposing (Grade, Result(..), School, Student, addStudent, allStudents, emptySchool, studentsInGrade)

import Dict exposing (..)


type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict Int (List Student)


type Result
    = Added
    | Duplicate


emptySchool : School
emptySchool =
    Dict.empty


addStudent : Grade -> Student -> School -> ( Result, School )
addStudent grade student school =
    let
        isDuplicate =
            Dict.values school |> List.concat |> List.member student

        updateSchool : Maybe (List Student) -> Maybe (List Student)
        updateSchool _ =
            List.append (studentsInGrade grade school) [ student ] |> Just
    in
    if isDuplicate then
        ( Duplicate, school )

    else
        ( Added, Dict.update grade updateSchool school )


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    Dict.get grade school |> Maybe.withDefault [] |> List.sort


allStudents : School -> List Student
allStudents =
    Dict.values >> List.map List.sort >> List.concat
