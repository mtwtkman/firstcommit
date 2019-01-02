module Main exposing (Model, Msg(..), init, main, update, view)

import Browser as Browser
import Debug exposing (log)
import GraphQL.Client.Http as GraphQLClient
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import Html exposing (Html, a, br, button, div, input, text)
import Html.Attributes exposing (href, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (header)
import Task exposing (Task)



-- GRAPHQL


owner : Var.Variable { vars | owner : String }
owner =
    Var.required "owner" .owner Var.string


name : Var.Variable { vars | name : String }
name =
    Var.required "name" .name Var.string


qualifiedName : Var.Variable { vars | qualifiedName : String }
qualifiedName =
    Var.required "qualifiedName" .qualifiedName Var.string


commitFragment : SelectionSpec Field result vars -> Fragment result vars
commitFragment historySpec =
    fragment "commitFragment"
        (onType "Commit")
        (extract historySpec)


history : List ( String, Arg.Value vars ) -> ValueSpec NonNull ObjectType result vars -> SelectionSpec Field result vars
history args inner =
    field "history" args inner


type alias RepositorySpec result vars =
    ValueSpec NonNull ObjectType (Maybe result) vars


ref : ValueSpec NonNull ObjectType result { vars | qualifiedName : String } -> ValueSpec NonNull ObjectType result { vars | qualifiedName : String }
ref targetSpec =
    extract
        (field "ref"
            [ ( "qualifiedName", Arg.variable qualifiedName ) ]
            targetSpec
        )


repository : ValueSpec NonNull ObjectType result { vars | name : String, owner : String, qualifiedName : String } -> RepositorySpec result { vars | name : String, owner : String, qualifiedName : String }
repository targetSpec =
    extract
        (field "repository"
            [ ( "owner", Arg.variable owner )
            , ( "name", Arg.variable name )
            ]
            (nullable (ref targetSpec))
        )


queryRequest : RepositorySpec result vars -> vars -> Request Query (Maybe result)
queryRequest repositorySpec args =
    queryDocument repositorySpec
        |> request args


type alias CommitSummary =
    { cursor : List String
    , totalCount : Int
    }


commitSummaryRequest : Model -> Request Query (Maybe CommitSummary)
commitSummaryRequest model =
    let
        historyFirst =
            Var.required "first" .first Var.int

        totalCount : SelectionSpec Field Int vars
        totalCount =
            field "totalCount" [] int

        edges : SelectionSpec Field (List String) vars
        edges =
            field "edges" [] (list (extract (field "cursor" [] string)))

        historyArgs =
            [ ( "first", Arg.variable historyFirst ) ]

        historyInner : ValueSpec NonNull ObjectType CommitSummary vars
        historyInner =
            object CommitSummary
                |> with edges
                |> with totalCount

        target : ValueSpec NonNull ObjectType CommitSummary { vars | first : Int }
        target =
            extract
                (field "target"
                    []
                    (extract
                        (assume
                            (fragmentSpread
                                (history historyArgs historyInner |> commitFragment)
                            )
                        )
                    )
                )
    in
    queryRequest
        (repository target)
        { name = model.name
        , owner = model.owner
        , qualifiedName = model.qualifiedName
        , first = 1
        }


type alias InitialCommit =
    { commitUrl : String
    , message : String
    }


initialCommitRequest : Model -> CommitSummary -> Request Query (Maybe (List InitialCommit))
initialCommitRequest model commitSummary =
    let
        historyBefore =
            Var.required "before" .before Var.string

        historyLast =
            Var.required "last" .last Var.int

        historyArgs =
            [ ( "before", Arg.variable historyBefore )
            , ( "last", Arg.variable historyLast )
            ]

        commitUrl =
            field "commitUrl" [] string

        message =
            field "message" [] string

        node =
            field "node"
                []
                (object InitialCommit
                    |> with commitUrl
                    |> with message
                )

        edges =
            field "edges" [] (list (extract node))

        historyInner =
            extract edges

        target =
            extract
                (field "target"
                    []
                    (extract
                        (assume
                            (fragmentSpread
                                (history historyArgs historyInner |> commitFragment)
                            )
                        )
                    )
                )

        cursor =
            List.head commitSummary.cursor |> Maybe.withDefault ""

        before =
            Maybe.map2 (\a -> \b -> a ++ " " ++ b)
                (String.split " " cursor |> List.head)
                (Just (String.fromInt commitSummary.totalCount))
                |> Maybe.withDefault ""
    in
    queryRequest
        (repository target)
        { name = model.name
        , owner = model.owner
        , qualifiedName = model.qualifiedName
        , before = before
        , last = 1
        }



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


type alias Model =
    { owner : String
    , name : String
    , qualifiedName : String
    , apiToken : String
    , initialCommit : Maybe InitialCommit
    , fetching : Bool
    , errorMessage : String
    }


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
