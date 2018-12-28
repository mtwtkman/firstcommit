module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Github.Object
import Github.Object.Repository as Repository
import Github.Query as Query
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (br, button, div, h1, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData)



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- TYPE


type Msg
    = InputName String
    | InputOwner String
    | InputApiToken String
    | FetchCommitCount
    | FetchedCommitCount CommitCountResponse


type alias CommitCountResponse =
    RemoteData (Graphql.Http.Error Response) Response


type alias Model =
    { name : String
    , owner : String
    , apiToken : String
    , qualifiedName : String
    , commitCount : Int
    }



-- Graphql


type alias CommitHistory =
    { totalCount : Int
    }


type alias RepositoryRefTarget =
    { target : CommitHistory
    }


type alias RepositoryRef =
    { ref : RepositoryRefTarget
    }


type alias Response =
    { repository : RepositoryRef
    }


query : Model -> SelectionSet Response RootQuery
query model =
    SelectionSet.succeed Response
        |> with
            (Query.repository { owner = model.owner, name = model.name } (repo model)
                |> SelectionSet.nonNullOrFail
            )


repo : Model -> SelectionSet RepositoryRef Github.Object.Repository
repo model =
    SelectionSet.succeed RepositoryRef
        |> with (Repository.ref { qualifiedName = model.qualifiedName })



-- HTTP


makeRequest : Model -> Cmd Msg
makeRequest model =
    query
        |> Graphql.Http.queryRequest "https://api.github.com/graphql" query
        |> Graphql.Http.withHeader "Authorization" ("Bearer " ++ model.apiToken)
        |> Graphql.Http.send (RemoteData.fromResult >> FetchedCommitCount)



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = ""
      , owner = ""
      , apiToken = ""
      , qualifiedName = ""
      , commitCount = 0
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        InputName newName ->
            ( { model | name = newName }, Cmd.none )

        InputOwner newOwner ->
            ( { model | owner = newOwner }, Cmd.none )

        InputApiToken newApiToken ->
            ( { model | apiToken = newApiToken }, Cmd.none )

        FetchCommitCount ->
            ( model, makeRequest model )

        FetchedCommitCount response ->
            ( { model | commitCount = response.ref.target.totalCount }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "firstcommit"
    , body =
        [ div []
            [ input [ placeholder "api token", value model.apiToken, onInput InputApiToken ] []
            , br [] []
            , input [ placeholder "name", value model.name, onInput InputName ] []
            , br [] []
            , input [ placeholder "owner", value model.owner, onInput InputOwner ] []
            , br [] []
            , input [ placeholder "qualifiedName", value model.qualifiedName, onInput InputOwner ] []
            , br [] []
            , button [ onClick FetchCommitCount ] [ text "🍣" ]
            ]
        , div []
            [ text <| "api token: " ++ model.apiToken
            , br [] []
            , text <| "name: " ++ model.name
            , br [] []
            , text <| "owner: " ++ model.owner
            , br [] []
            , text <| "qualifiedName: " ++ model.qualifiedName
            , br [] []
            , text <| String.fromInt model.commitCount
            ]
        ]
    }
