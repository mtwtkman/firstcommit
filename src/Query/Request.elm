module Query.Request exposing (commitSummaryRequest, initialCommitRequest)

import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import Query.Var exposing (..)
import Type exposing (..)


buildRequest : List ( String, Arg.Value { vars | owner : String, name : String, qualifiedName : String } ) -> ValueSpec NonNull ObjectType result { vars | owner : String, name : String, qualifiedName : String } -> ValueSpec NonNull ObjectType (Maybe result) { vars | owner : String, name : String, qualifiedName : String }
buildRequest args inner =
    extract
        (field "repository"
            [ ( "owner", Arg.variable ownerVar )
            , ( "name", Arg.variable nameVar )
            ]
            (nullable
                (extract
                    (field "ref"
                        [ ( "qualifiedName", Arg.variable qualifiedNameVar ) ]
                        (extract
                            (field "target"
                                []
                                (extract
                                    (assume
                                        (fragmentSpread
                                            (fragment "commitFragment"
                                                (onType "Commit")
                                                (extract
                                                    (field "history" args inner)
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


commitSummaryRequest : Model -> Request Query (Maybe CommitSummary)
commitSummaryRequest model =
    buildRequest
        [ ( "first", Arg.variable firstVar ) ]
        (object CommitSummary
            |> with (field "edges" [] (list (extract (field "cursor" [] string))))
            |> with (field "totalCount" [] int)
        )
        |> queryDocument
        |> request
            { owner = model.owner
            , name = model.name
            , qualifiedName = model.qualifiedName
            , first = 1
            }


initialCommitRequest : Model -> CommitSummary -> Request Query (Maybe (List InitialCommit))
initialCommitRequest model commitSummary =
    let
        before =
            Maybe.map2 (\a -> \b -> a ++ " " ++ b)
                (String.split " " (List.head commitSummary.cursor |> Maybe.withDefault "") |> List.head)
                (Just (String.fromInt commitSummary.totalCount))
                |> Maybe.withDefault ""
    in
    buildRequest
        [ ( "before", Arg.variable beforeVar )
        , ( "last", Arg.variable lastVar )
        ]
        (extract
            (field "edges"
                []
                (list
                    (extract
                        (field "node"
                            []
                            (object InitialCommit
                                |> with (field "commitUrl" [] string)
                                |> with (field "message" [] string)
                            )
                        )
                    )
                )
            )
        )
        |> queryDocument
        |> request
            { name = model.name
            , owner = model.owner
            , qualifiedName = model.qualifiedName
            , before = before
            , last = 1
            }
