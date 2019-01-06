module Main exposing (Msg(..), init, main, update, view)

import Browser as Browser
import Debug exposing (log)
import GraphQL.Client.Http as GraphQLClient
import GraphQL.Request.Builder exposing (..)
import Helper exposing (..)
import Html exposing (Html, a, br, button, div, input, text)
import Html.Attributes exposing (disabled, href, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (header)
import Query.Request exposing (..)
import Task exposing (Task)
import Type exposing (..)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias CommitSummaryResponse =
    Result GraphQLClient.Error (Maybe CommitSummary)


type alias InitialCommitResponse =
    Result GraphQLClient.Error (Maybe (List InitialCommit))


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



-- UPDATE


type Msg
    = UpdateOwner String
    | UpdateName String
    | UpdateBranch String
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



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        toBranch : Maybe String -> Maybe String
        toBranch qualifiedName =
            let
                f : String -> Maybe String
                f =
                    \s -> String.split "/" s |> List.reverse |> List.head
            in
            Maybe.andThen f qualifiedName
    in
    { title = "firstcommit"
    , body =
        [ div []
            [ div []
                [ requiredInput "owner" UpdateOwner model.form.owner
                , br [] []
                , requiredInput "name" UpdateName model.form.name
                , br [] []
                , requiredInput "branch" UpdateBranch (toBranch model.form.qualifiedName)
                , br [] []
                , requiredInput "apiToken" UpdateApiToken model.form.apiToken
                ]
            , div []
                (List.map
                    (\( l, n ) -> div [] [ labeledText l (n model.form) ])
                    [ ( "owner", .owner ), ( "name", .name ), ( "qualifiedName", .qualifiedName ), ( "apiToken", .apiToken ) ]
                )
            , div []
                [ button [ onClick SendCommitSummaryRequest, disabled (not model.sendable) ] [ text "get first commit" ]
                ]
            , div []
                [ case model.fetching of
                    NotFetching ->
                        text ""

                    Done ->
                        case model.initialCommit of
                            Just v ->
                                resultView v

                            Nothing ->
                                text "Cannot find"

                    _ ->
                        text "fetching..."
                ]
            ]
        ]
    }


requiredInput : String -> (String -> msg) -> Maybe String -> Html msg
requiredInput ph msg val =
    div []
        [ input [ placeholder ph, onInput msg, value (unwrap val) ] []
        , text <|
            case val of
                Just _ ->
                    ""

                Nothing ->
                    "required"
        ]


labeledText : String -> Maybe String -> Html msg
labeledText label value =
    text (label ++ ": " ++ Maybe.withDefault "" value)


resultView : InitialCommit -> Html msg
resultView v =
    div []
        [ div []
            [ a [ href v.commitUrl ]
                [ "commitUrl: " ++ v.commitUrl |> text ]
            ]
        , div [] [ "message: " ++ v.message |> text ]
        ]
