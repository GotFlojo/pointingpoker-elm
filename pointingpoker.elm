module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (..)


-- import Maybe.Extra exposing (..)
-- Model


type alias User =
    { name : String
    , vote : Maybe Int
    , id : Int
    }



--Todo handle  empty String as name


mkUser : String -> User
mkUser name =
    { name = name
    , vote = Nothing
    , id = 0
    }


type alias UserList =
    Dict Int User


type alias Model =
    { story : String
    , users : Dict Int User
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


addUser : Int -> User -> UserList -> UserList
addUser id user users =
    Dict.insert id { user | id = id } users


addUserToSession : ( Int, User ) -> Model -> Model
addUserToSession ( id, user ) session =
    { session | users = addUser id user session.users }


asUserInSession : Model -> ( Int, User ) -> Model
asUserInSession =
    flip addUserToSession


deleteUser : Int -> UserList -> UserList
deleteUser userId users =
    Dict.remove userId users


doVote : Int -> User -> User
doVote newVote user =
    { user | vote = (Just newVote) }


clearVote : User -> User
clearVote user =
    { user | vote = Nothing }


getVotes : UserList -> List (Maybe Int)
getVotes session =
    let
        users =
            Dict.values session
    in
        List.map (\user -> user.vote) users


getUsernames : UserList -> List String
getUsernames session =
    let
        users =
            Dict.values session
    in
        List.map (\user -> user.name) users


hasVoted : User -> Bool
hasVoted user =
    case user.vote of
        Nothing ->
            False

        Just _ ->
            True


votingDone : UserList -> Bool
votingDone session =
    let
        users =
            Dict.values session
    in
        List.all hasVoted users


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
            let
                unwrap =
                    (\n ->
                        case n of
                            Nothing ->
                                0

                            Just i ->
                                i + 1
                    )

                newId =
                    model.users |> Dict.keys |> List.maximum |> unwrap
            in
                addUserToSession ( newId, (mkUser name) ) model

        DeleteUser name ->
            { model | users = deleteUser name.id model.users }

        UpdateNameInput name ->
            { model | userNameInput = name }

        _ ->
            model



-- View


renderUserList : UserList -> Html Msg
renderUserList users =
    let
        listItem user =
            li []
                [ text user.name
                , button [ onClick (DeleteUser user) ] [ text "x" ]
                , div []
                    [ renderVotingOptions
                    , button [ onClick (Vote user 1) ] [ text "Vote" ]
                    ]
                ]
    in
        ul [] <| List.map listItem (Dict.values users)


renderVotingOptions : Html Msg
renderVotingOptions =
    let
        mkOption listItem =
            option [] [ listItem |> toString |> text ]

        default =
            option [ selected True, hidden True, disabled True ] [ text "Points" ]
    in
        select [] <| default :: (List.map mkOption [ 0, 1, 2, 3, 5, 8, 13, 20, 40, 100 ])


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


init : Model
init =
    Model "" Dict.empty votingDone 0.0 "" "" ""


main : Program Never Model Msg
main =
    beginnerProgram { model = init, view = view, update = update }
