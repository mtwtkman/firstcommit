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


commitSummaryRequest : String -> String -> String -> Request Query (Maybe CommitSummary)
commitSummaryRequest owner name qualifiedName =
    buildRequest
        [ ( "first", Arg.variable firstVar ) ]
        (object CommitSummary
            |> with (field "edges" [] (list (extract (field "cursor" [] string))))
            |> with (field "totalCount" [] int)
        )
        |> queryDocument
        |> request
            { owner = owner
            , name = name
            , qualifiedName = qualifiedName
            , first = 1
            }


initialCommitRequest : String -> String -> String -> CommitSummary -> Request Query (Maybe (List InitialCommit))
initialCommitRequest owner name qualifiedName commitSummary =
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
            { owner = owner
            , name = name
            , qualifiedName = qualifiedName
            , before = before
            , last = 1
            }
