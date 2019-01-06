module Main exposing (Msg(..), init, main, update, view)

import Browser as Browser
import Debug exposing (log)
import GraphQL.Client.Http as GraphQLClient
import GraphQL.Request.Builder exposing (..)
import Html exposing (Html, a, br, button, div, input, text)
import Html.Attributes exposing (href, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (header)
import Query.Request exposing (..)
import Task exposing (Task)
import Type exposing (..)



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias CommitSummaryResponse =
    Result GraphQLClient.Error (Maybe CommitSummary)


type alias InitialCommitResponse =
    Result GraphQLClient.Error (Maybe (List InitialCommit))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { owner = "mtwtkman"
      , name = "editor"
      , qualifiedName = "refs/heads/master"
      , apiToken = ""
      , initialCommit = Nothing
      , fetching = False
      , errorMessage = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateOwner String
    | UpdateName String
    | UpdateQualifiedName String
    | UpdateApiToken String
    | SendCommitSummaryRequest
    | ReceiveCommitSummaryResponse CommitSummaryResponse
    | ReceiveInitialCommitResponse InitialCommitResponse


sendOption : String -> GraphQLClient.RequestOptions
sendOption apiToken =
    { method = "POST"
    , headers =
        [ header "Authorization" ("Bearer " ++ apiToken)
        ]
    , url = "https://api.github.com/graphql"
    , timeout = Nothing
    , withCredentials = False
    }


sendQueryCommitSummaryRequest : String -> Request Query (Maybe CommitSummary) -> Cmd Msg
sendQueryCommitSummaryRequest apiToken request =
    GraphQLClient.customSendQuery
        (sendOption apiToken)
        request
        |> Task.attempt ReceiveCommitSummaryResponse


sendQueryInitialCommitRequest : String -> Request Query (Maybe (List InitialCommit)) -> Cmd Msg
sendQueryInitialCommitRequest apiToken request =
    GraphQLClient.customSendQuery
        (sendOption apiToken)
        request
        |> Task.attempt ReceiveInitialCommitResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateOwner value ->
            ( { model | owner = value }, Cmd.none )

        UpdateName value ->
            ( { model | name = value }, Cmd.none )

        UpdateQualifiedName value ->
            ( { model | qualifiedName = value }, Cmd.none )

        UpdateApiToken value ->
            ( { model | apiToken = value }, Cmd.none )

        SendCommitSummaryRequest ->
            let
                errorMessage =
                    if
                        List.any String.isEmpty
                            [ model.owner
                            , model.name
                            , model.qualifiedName
                            , model.apiToken
                            ]
                    then
                        "cannot send"

                    else
                        ""
            in
            ( { model | errorMessage = errorMessage, fetching = True }
            , sendQueryCommitSummaryRequest model.apiToken (commitSummaryRequest model)
            )

        ReceiveCommitSummaryResponse response ->
            ( model
            , case Result.withDefault Nothing response of
                Just data ->
                    sendQueryInitialCommitRequest model.apiToken (initialCommitRequest model data)

                Nothing ->
                    Cmd.none
            )

        ReceiveInitialCommitResponse response ->
            ( { model
                | initialCommit = Result.withDefault Nothing response |> Maybe.map List.head |> Maybe.withDefault Nothing
                , fetching = False
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "firstcommit"
    , body =
        [ div []
            [ div []
                [ input [ placeholder "owner", onInput UpdateOwner, value model.owner ] []
                , br [] []
                , input [ placeholder "name", onInput UpdateName, value model.name ] []
                , br [] []
                , input [ placeholder "qualifiedName", onInput UpdateQualifiedName, value model.qualifiedName ] []
                , br [] []
                , input [ placeholder "apiToken", onInput UpdateApiToken, value model.apiToken ] []
                ]
            , div []
                [ labeledText "owner" model.owner
                , br [] []
                , labeledText "name" model.name
                , br [] []
                , labeledText "qualifiedName" model.qualifiedName
                , br [] []
                , labeledText "apiToken" model.apiToken
                ]
            , div []
                [ button [ onClick SendCommitSummaryRequest ] [ text "get first commit" ]
                ]
            , div []
                [ text model.errorMessage ]
            , case model.fetching of
                True ->
                    div [] [ text "fetching..." ]

                False ->
                    case model.initialCommit of
                        Just v ->
                            resultView v

                        Nothing ->
                            div [] [ text "not found" ]
            ]
        ]
    }


labeledText : String -> String -> Html msg
labeledText label value =
    text (label ++ ": " ++ value)


resultView : InitialCommit -> Html msg
resultView v =
    div []
        [ div []
            [ a [ href v.commitUrl ]
                [ "commitUrl: " ++ v.commitUrl |> text ]
            ]
        , div [] [ "message: " ++ v.message |> text ]
        ]
