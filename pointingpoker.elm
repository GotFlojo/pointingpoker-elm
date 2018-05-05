module Main exposing (..)

-- import Maybe.Extra exposing (..)


type alias User =
    { name : String
    , vote : Maybe Int
    }


mkUser : String -> User
mkUser name =
    { name = name
    , vote = Nothing
    }


type alias Session =
    List User


addUser : Session -> User -> Maybe Session
addUser session user =
    case List.member user session of
        False ->
            Just (user :: session)

        _ ->
            Nothing


hasVoted : User -> Bool
hasVoted user =
    case user.vote of
        Nothing ->
            False

        Just _ ->
            True


doVote : User -> Int -> User
doVote user newVote =
    { user | vote = (Just newVote) }


clearVote : User -> User
clearVote user =
    { user | vote = Nothing }


getVotes : Session -> List (Maybe Int)
getVotes session =
    List.map (\user -> user.vote) session


getUsernames : Session -> List String
getUsernames session =
    List.map (\user -> user.name) session


votingDone : Session -> Bool
votingDone session =
    List.all hasVoted session


getVoteValue : Maybe Int -> Int
getVoteValue vote =
    case vote of
        Nothing ->
            0

        Just value ->
            value


getCountableVotes : List (Maybe Int) -> List Int
getCountableVotes votes =
    let
        isCountable vote =
            case vote of
                Nothing ->
                    False

                _ ->
                    True
    in
        votes
            |> List.filter isCountable
            |> List.map getVoteValue


maxVote votes =
    List.maximum votes


minVote votes =
    List.minimum votes
