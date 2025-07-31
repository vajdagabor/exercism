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
        updatedGrade () =
            student :: studentsInGrade grade school |> List.sort
    in
    if allStudents school |> List.member student then
        ( Duplicate, school )

    else
        ( Added, Dict.insert grade (updatedGrade ()) school )


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade =
    Dict.get grade >> Maybe.withDefault []


allStudents : School -> List Student
allStudents =
    Dict.values >> List.concat
