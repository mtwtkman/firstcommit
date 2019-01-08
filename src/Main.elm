module Main exposing (main)

import Browser as Browser
import GraphQL.Client.Http as GraphQLClient
import GraphQL.Request.Builder exposing (..)
import Helper exposing (..)
import Http exposing (header)
import Msg exposing (..)
import Query.Request exposing (..)
import Task exposing (Task)
import Type exposing (..)
import View exposing (view)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        form =
            { owner = Nothing
            , name = Nothing
            , qualifiedName = Just "refs/heads/master"
            , apiToken = Nothing
            }
    in
    ( { form = form
      , fetching = NotFetching
      , sendable = False
      , initialCommit = Nothing
      }
    , Cmd.none
    )


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
    let
        isSendable : Form -> Bool
        isSendable form =
            List.all isNothing [ form.owner, form.name, form.qualifiedName, form.apiToken ]
    in
    case msg of
        UpdateOwner value ->
            let
                old =
                    model.form

                new =
                    { old | owner = emptyToNothing value }
            in
            ( { model | form = new, sendable = isSendable new }, Cmd.none )

        UpdateName value ->
            let
                old =
                    model.form

                new =
                    { old | name = emptyToNothing value }
            in
            ( { model | form = new, sendable = isSendable new }, Cmd.none )

        UpdateBranch value ->
            let
                old =
                    model.form

                new =
                    { old | qualifiedName = emptyToNothing value |> Maybe.map (\s -> "refs/heads/" ++ s) }
            in
            ( { model | form = new, sendable = isSendable new }, Cmd.none )

        UpdateApiToken value ->
            let
                old =
                    model.form

                new =
                    { old | apiToken = emptyToNothing value }
            in
            ( { model | form = new, sendable = isSendable new }, Cmd.none )

        SendCommitSummaryRequest ->
            if model.sendable then
                let
                    owner =
                        unwrap model.form.owner

                    name =
                        unwrap model.form.name

                    qualifiedName =
                        unwrap model.form.qualifiedName
                in
                ( { model | fetching = FetchingCommitSummary, initialCommit = Nothing }
                , sendQueryCommitSummaryRequest
                    (unwrap model.form.apiToken)
                    (commitSummaryRequest owner name qualifiedName)
                )

            else
                ( model, Cmd.none )

        ReceiveCommitSummaryResponse response ->
            case Result.withDefault Nothing response of
                Just data ->
                    let
                        owner =
                            unwrap model.form.owner

                        name =
                            unwrap model.form.name

                        qualifiedName =
                            unwrap model.form.qualifiedName
                    in
                    ( { model | fetching = FetchingInitialCommit }
                    , sendQueryInitialCommitRequest (unwrap model.form.apiToken)
                        (initialCommitRequest owner name qualifiedName data)
                    )

                Nothing ->
                    ( { model | fetching = Done }, Cmd.none )

        ReceiveInitialCommitResponse response ->
            ( { model
                | initialCommit = Result.withDefault Nothing response |> Maybe.map List.head |> Maybe.withDefault Nothing
                , fetching = Done
              }
            , Cmd.none
            )
