module Main exposing (Model, Msg(..), init, main, update, view)

import Browser as Browser
import GraphQL.Client.Http as GraphQLClient
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import Html exposing (Html, br, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (header)
import Task exposing (Task)



-- GRAPHQL


type alias CommitSummary =
    { latestCursor : List String
    , totalCount : Int
    }


commitSummaryRequest : Model -> Request Query (Maybe CommitSummary)
commitSummaryRequest model =
    let
        owner =
            Var.required "owner" .owner Var.string

        name =
            Var.required "name" .name Var.string

        qualifiedName =
            Var.required "qualifiedName" .qualifiedName Var.string

        historyFirst =
            Var.required "first" .first Var.int

        totalCount =
            field "totalCount" [] int

        edges =
            field "edges" [] (list (extract (field "cursor" [] string)))

        history =
            field "history"
                [ ( "first", Arg.variable historyFirst ) ]
                (object CommitSummary
                    |> with edges
                    |> with totalCount
                )

        commitFragment =
            fragment "commitFragment"
                (onType "Commit")
                (extract history)

        target =
            extract
                (field "target"
                    []
                    (extract (assume (fragmentSpread commitFragment)))
                )

        ref =
            extract
                (field "ref"
                    [ ( "qualifiedName", Arg.variable qualifiedName ) ]
                    target
                )
    in
    extract
        (field "repository"
            [ ( "owner", Arg.variable owner )
            , ( "name", Arg.variable name )
            ]
            (nullable ref)
        )
        |> queryDocument
        |> request
            { owner = model.owner
            , name = model.name
            , qualifiedName = model.qualifiedName
            , first = 1
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


type alias Model =
    { owner : String
    , name : String
    , qualifiedName : String
    , apiToken : String
    , commitSummary : Maybe CommitSummary
    , fetching : Bool
    , errorMessage : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { owner = "mtwtkman"
      , name = "editor"
      , qualifiedName = "refs/heads/master"
      , apiToken = "f4dbcaaf6ebfea54d3a3e229c8f995c7cd0ad850"
      , commitSummary = Nothing
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
    | SendRequest
    | ReceiveCommitSummaryResponse CommitSummaryResponse


sendQueryCommitSummaryRequest : String -> Request Query (Maybe CommitSummary) -> Cmd Msg
sendQueryCommitSummaryRequest apiToken request =
    GraphQLClient.customSendQuery
        { method = "POST"
        , headers =
            [ header "Authorization" ("Bearer " ++ apiToken)
            ]
        , url = "https://api.github.com/graphql"
        , timeout = Nothing
        , withCredentials = False
        }
        request
        |> Task.attempt ReceiveCommitSummaryResponse


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

        SendRequest ->
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
            let
                v =
                    case Result.toMaybe response of
                        Just resp ->
                            resp

                        Nothing ->
                            Nothing
            in
            ( { model | commitSummary = v, fetching = False }, Cmd.none )



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
                [ button [ onClick SendRequest ] [ text "🍣" ]
                ]
            , div []
                [ text model.errorMessage ]
            , case model.commitSummary of
                Just v ->
                    resultView v

                Nothing ->
                    case model.fetching of
                        True ->
                            div [] [ text "fetching..." ]

                        False ->
                            div [] []
            ]
        ]
    }


labeledText : String -> String -> Html msg
labeledText label value =
    text (label ++ ": " ++ value)


resultView : CommitSummary -> Html msg
resultView v =
    let
        latestCursor =
            case List.head v.latestCursor of
                Just h ->
                    h

                Nothing ->
                    "not found"
    in
    div []
        [ div [] [ "totalCount: " ++ String.fromInt v.totalCount |> text ]
        , div [] [ "cursor: " ++ latestCursor |> text ]
        ]
