module View exposing (view)

import Browser as Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Columns exposing (column, columnModifiers)
import Bulma.Elements exposing (button, buttonModifiers)
import Bulma.Layout exposing (container)
import Bulma.Modifiers exposing (Color(..))
import Helper exposing (unwrap)
import Html exposing (Html, a, div, input, text)
import Html.Attributes exposing (disabled, href, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Msg exposing (..)
import Type exposing (..)


view : Model -> Browser.Document Msg
view model =
    { title = "firstcommit"
    , body =
        [ container []
            [ stylesheet
            , column
                columnModifiers
                []
                [ inputRegion model
                , previewRegion model
                , getFirstCommitButton model.sendable
                , case model.fetching of
                    NotFetching ->
                        text ""

                    Done ->
                        case model.initialCommit of
                            Just v ->
                                resultView v

                            Nothing ->
                                div [] [ text "Cannot find" ]

                    _ ->
                        div [] [ text "fetching..." ]
                ]
            ]
        ]
    }


getFirstCommitButton : Bool -> Html Msg
getFirstCommitButton sendable =
    button
        { buttonModifiers
            | disabled = sendable
            , color = Primary
        }
        [ onClick SendCommitSummaryRequest
        ]
        [ text "get first commit" ]


inputRegion : Model -> Html Msg
inputRegion model =
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
    column
        columnModifiers
        []
        [ requiredInput "owner" UpdateOwner model.form.owner
        , requiredInput "name" UpdateName model.form.name
        , requiredInput "branch" UpdateBranch (toBranch model.form.qualifiedName)
        , requiredInput "apiToken" UpdateApiToken model.form.apiToken
        ]


requiredInput : String -> (String -> msg) -> Maybe String -> Html msg
requiredInput ph msg val =
    div []
        [ input
            [ onInput msg
            , placeholder ph
            , value (unwrap val)
            ]
            []
        , text <|
            case val of
                Just _ ->
                    ""

                Nothing ->
                    "required"
        ]


previewRegion : Model -> Html Msg
previewRegion model =
    div []
        ([ ( "owner", .owner )
         , ( "name", .name )
         , ( "qualifiedName", .qualifiedName )
         , ( "apiToken", .apiToken )
         ]
            |> List.map
                (\( l, f ) -> div [] [ text (l ++ ": " ++ unwrap (f model.form)) ])
        )


resultView : InitialCommit -> Html msg
resultView v =
    div []
        [ a [ href v.commitUrl ] [ text v.message ]
        ]
