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


type alias TotalCount =
    { totalCount : Int
    }


totalCountRequest : Model -> Request Query (Maybe TotalCount)
totalCountRequest model =
    let
        owner : Var.Variable { var | owner : String }
        owner =
            Var.required "owner" .owner Var.string

        name : Var.Variable { var | name : String }
        name =
            Var.required "name" .name Var.string

        qualifiedName : Var.Variable { var | qualifiedName : String }
        qualifiedName =
            Var.required "qualifiedName" .qualifiedName Var.string

        totalCount =
            field "totalCount" [] int

        history =
            field "history" [] (extract totalCount)

        commitFragment =
            fragment "commitFragment"
                (onType "Commit")
                (extract history)

        target =
            extract
                (field "target"
                    []
                    (object TotalCount
                        |> with
                            (assume (fragmentSpread commitFragment))
                    )
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


type alias TotalCountResponse =
    Result GraphQLClient.Error (Maybe TotalCount)


type alias Model =
    { owner : String
    , name : String
    , qualifiedName : String
    , apiToken : String
    , errorMessage : String
    , totalCount : Maybe TotalCount
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { owner = "mtwtkman"
      , name = "editor"
      , qualifiedName = "refs/heads/master"
      , apiToken = ""
      , totalCount = Nothing
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
    | ReceiveTotalCountResponse TotalCountResponse


sendQueryTotalCountRequest : String -> Request Query (Maybe TotalCount) -> Cmd Msg
sendQueryTotalCountRequest apiToken request =
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
        |> Task.attempt ReceiveTotalCountResponse


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
            ( { model | errorMessage = errorMessage }
            , sendQueryTotalCountRequest model.apiToken (totalCountRequest model)
            )

        ReceiveTotalCountResponse response ->
            let
                v =
                    case Result.toMaybe response of
                        Just resp ->
                            resp

                        Nothing ->
                            Nothing
            in
            ( { model | totalCount = v }, Cmd.none )



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
            , case model.totalCount of
                Just v ->
                    resultView v

                Nothing ->
                    div [] []
            ]
        ]
    }


labeledText : String -> String -> Html msg
labeledText label value =
    text (label ++ ": " ++ value)


resultView : TotalCount -> Html msg
resultView v =
    div []
        [ "Result :" ++ String.fromInt v.totalCount |> text ]
