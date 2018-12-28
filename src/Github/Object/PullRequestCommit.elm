-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.PullRequestCommit exposing (commit, id, pullRequest, resourcePath, url)

import Github.InputObject
import Github.Interface
import Github.Object
import Github.Scalar
import Github.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| The Git commit object
-}
commit : SelectionSet decodesTo Github.Object.Commit -> SelectionSet decodesTo Github.Object.PullRequestCommit
commit object_ =
    Object.selectionForCompositeField "commit" [] object_ identity


id : SelectionSet Github.Scalar.Id Github.Object.PullRequestCommit
id =
    Object.selectionForField "Scalar.Id" "id" [] (Object.scalarDecoder |> Decode.map Github.Scalar.Id)


{-| The pull request this commit belongs to
-}
pullRequest : SelectionSet decodesTo Github.Object.PullRequest -> SelectionSet decodesTo Github.Object.PullRequestCommit
pullRequest object_ =
    Object.selectionForCompositeField "pullRequest" [] object_ identity


{-| The HTTP path for this pull request commit
-}
resourcePath : SelectionSet Github.Scalar.Uri Github.Object.PullRequestCommit
resourcePath =
    Object.selectionForField "Scalar.Uri" "resourcePath" [] (Object.scalarDecoder |> Decode.map Github.Scalar.Uri)


{-| The HTTP URL for this pull request commit
-}
url : SelectionSet Github.Scalar.Uri Github.Object.PullRequestCommit
url =
    Object.selectionForField "Scalar.Uri" "url" [] (Object.scalarDecoder |> Decode.map Github.Scalar.Uri)
