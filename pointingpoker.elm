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
    , result : Float
    , userNameInput : String
    , userPointsInput : String
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
    | GetResult


addUser : User -> UserList -> Maybe UserList
addUser user session =
    case List.member user session of
        False ->
            Just (user :: session)

        _ ->
            Nothing


addUserToSession : User -> Model -> Model
addUserToSession user session =
    case (addUser user session.users) of
        Just userlist ->
            { session | users = userlist, userNameInput = "", error = "" }

        Nothing ->
            { session | error = "User with that name already exists", userNameInput = "" }


deleteUser : User -> UserList -> UserList
deleteUser user session =
    List.filter (\{ name } -> name /= user.name) session


doVote : Int -> User -> User
doVote newVote user =
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


hasVoted : User -> Bool
hasVoted user =
    case user.vote of
        Nothing ->
            False

        Just _ ->
            True


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
            addUserToSession (mkUser name) model

        DeleteUser name ->
            { model | users = deleteUser name model.users }

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
                    [ input [ type_ "number", Html.Attributes.min "0", placeholder "Points" ] []
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
            , button [ disabled (model.hasResult model.users) ] [ text "Get Result" ]
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
    Model "" [] votingDone 0.0 "" "" ""


main =
    beginnerProgram { model = init, view = view, update = update }
