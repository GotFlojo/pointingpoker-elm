module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- import Maybe.Extra exposing (..)
-- Model


type alias User =
    { name : String
    , vote : Maybe Int
    }



--Todo handle  empty String as name


mkUser : String -> User
mkUser name =
    { name = name
    , vote = Nothing
    }


type alias UserList =
    List User


type alias Model =
    { story : String
    , users : UserList
    , hasResult : UserList -> Bool
    , userNameInput : String
    , error : String
    }



-- Update


type Msg
    = NoOp
    | NewSession
    | NewStory String
    | ClearStory
    | AddUser String
    | DeleteUser User
    | Vote User Int
    | UpdateNameInput String


addUser : UserList -> User -> Maybe UserList
addUser session user =
    case List.member user session of
        False ->
            Just (user :: session)

        _ ->
            Nothing


addUserToSession : Model -> User -> Model
addUserToSession session user =
    case (addUser session.users user) of
        Just userlist ->
            { session | users = userlist, userNameInput = "", error = "" }

        Nothing ->
            { session | error = "User with that name already exists", userNameInput = "" }


deleteUser : UserList -> User -> UserList
deleteUser session user =
    List.filter (\{ name } -> name /= user.name) session


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


getVotes : UserList -> List (Maybe Int)
getVotes session =
    List.map (\user -> user.vote) session


getUsernames : UserList -> List String
getUsernames session =
    List.map (\user -> user.name) session


votingDone : UserList -> Bool
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


maxVote : List Int -> Maybe.Maybe Int
maxVote votes =
    List.maximum votes


minVote : List Int -> Maybe.Maybe Int
minVote votes =
    List.minimum votes


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewSession ->
            init

        NewStory newStory ->
            { model | story = newStory }

        ClearStory ->
            { model | story = "" }

        AddUser name ->
            addUserToSession model (mkUser name)

        DeleteUser name ->
            { model | users = deleteUser model.users name }

        UpdateNameInput name ->
            { model | userNameInput = name }

        _ ->
            model


renderUserList : UserList -> Html Msg
renderUserList users =
    let
        listItem user =
            li []
                [ text user.name
                , button [ onClick (DeleteUser user) ] [ text "x" ]
                , div []
                    [ input [ placeholder "Points" ] []
                    , button [ onClick (Vote user 1) ] [ text "Vote User" ]
                    ]
                ]
    in
        ul [] <| List.map listItem users



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [] [ input [ placeholder "New Story", Html.Attributes.value model.story, onInput NewStory ] [] ]
        , div []
            [ renderUserList model.users
            , div []
                [ input [ placeholder "Username", Html.Attributes.value model.userNameInput, onInput UpdateNameInput ] []
                , button [ onClick (AddUser model.userNameInput) ] [ text "Add User" ]
                ]
            ]
        , div
            []
            [ button [ onClick NewSession ] [ text "New Session" ]
            , button [] [ text "Get Result" ]
            ]
        , div [] <|
            if String.isEmpty model.error then
                []
            else
                [ text model.error ]
        ]



--Html.beginnerProgram
--main =
--   view (Session "" [ mkUser "Flori" ] votingDone)


init : Model
init =
    Model "" [] votingDone "" ""


main =
    beginnerProgram { model = init, view = view, update = update }
