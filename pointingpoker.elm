module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String exposing (..)
import Random exposing (..)

main =
    Html.beginnerProgram 
        { model = init
        , update = update
        , view = view
        }

-- model
type alias Vote = 
    { points: Int
    , visible: Bool
    }
newVote : Vote 
newVote = 
    { points = 0
    , visible = True
    }
type alias User =
    { name: String
    , vote: Vote
    , hasVoted: Bool
    , id: Int
    }
newUser : String -> User
newUser name =
    { name = name
    , vote = newVote
    , hasVoted = False
    , id = 0
    }

type alias Model = 
    { id: String
    , lastUser: String
    , users: List User
    , story: String
    , votingDone: Bool
    }
    
init : Model
init = 
    { id = "0"
    , lastUser = ""
    , users = []
    , story = ""
    , votingDone = False
    }

-- update
type Msg 
    = AddUser
    | DeleteUser String
    | EditCurrentUser String
    | NewStory String
    | ClearVote
    | NewSession
    | NewUserId Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddUser -> {model | users = (newUser model.lastUser) :: model.users}
        EditCurrentUser name -> {model | lastUser = name}
        NewStory newStory -> {model | story = newStory}
        NewSession -> init
        _ -> model 


-- view
view : Model -> Html Msg
view model =
    div [] 
        [ div [] [ text (toString model)]
        , div [] [ input [ type_ "text", placeholder "New Story", onInput NewStory] []]
        , div [] [ input [ type_ "text", placeholder "New User", onInput EditCurrentUser] []
                 , button [onClick AddUser] [text "+"]
                 ]
        , div [] [ button [onClick NewSession] [text "Clear"]]
        ]
