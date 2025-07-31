module TicketPlease exposing (..)

import TicketPleaseSupport exposing (Status(..), Ticket(..), User(..))


emptyComment : ( User, String ) -> Bool
emptyComment =
    Tuple.second >> String.isEmpty


numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments (Ticket { createdBy, comments }) =
    let
        ( creator, _ ) =
            createdBy

        isCreator user =
            user == creator

        users =
            List.map Tuple.first comments
    in
    List.filter isCreator users |> List.length


assignedToDevTeam : Ticket -> Bool
assignedToDevTeam (Ticket { assignedTo }) =
    case assignedTo of
        Just (User name) ->
            List.member name [ "Alice", "Bob", "Charlie" ]

        _ ->
            False


assignTicketTo : User -> Ticket -> Ticket
assignTicketTo user (Ticket ({ status } as ticket)) =
    case status of
        New ->
            Ticket { ticket | assignedTo = Just user, status = InProgress }

        Archived ->
            Ticket ticket

        _ ->
            Ticket { ticket | assignedTo = Just user }
