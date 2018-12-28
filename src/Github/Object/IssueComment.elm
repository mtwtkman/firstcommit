-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.IssueComment exposing (ReactionsOptionalArguments, UserContentEditsOptionalArguments, author, authorAssociation, body, bodyHTML, bodyText, createdAt, createdViaEmail, databaseId, editor, id, issue, lastEditedAt, publishedAt, pullRequest, reactionGroups, reactions, repository, resourcePath, updatedAt, url, userContentEdits, viewerCanDelete, viewerCanReact, viewerCanUpdate, viewerCannotUpdateReasons, viewerDidAuthor)

import Github.Enum.CommentAuthorAssociation
import Github.Enum.CommentCannotUpdateReason
import Github.Enum.ReactionContent
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


{-| The actor who authored the comment.
-}
author : SelectionSet decodesTo Github.Interface.Actor -> SelectionSet (Maybe decodesTo) Github.Object.IssueComment
author object_ =
    Object.selectionForCompositeField "author" [] object_ (identity >> Decode.nullable)


{-| Author's association with the subject of the comment.
-}
authorAssociation : SelectionSet Github.Enum.CommentAuthorAssociation.CommentAuthorAssociation Github.Object.IssueComment
authorAssociation =
    Object.selectionForField "Enum.CommentAuthorAssociation.CommentAuthorAssociation" "authorAssociation" [] Github.Enum.CommentAuthorAssociation.decoder


{-| Identifies the comment body.
-}
body : SelectionSet String Github.Object.IssueComment
body =
    Object.selectionForField "String" "body" [] Decode.string


{-| The comment body rendered to HTML.
-}
bodyHTML : SelectionSet Github.Scalar.Html Github.Object.IssueComment
bodyHTML =
    Object.selectionForField "Scalar.Html" "bodyHTML" [] (Object.scalarDecoder |> Decode.map Github.Scalar.Html)


{-| Identifies the body of the issue rendered to text.
-}
bodyText : SelectionSet String Github.Object.IssueComment
bodyText =
    Object.selectionForField "String" "bodyText" [] Decode.string


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Github.Scalar.DateTime Github.Object.IssueComment
createdAt =
    Object.selectionForField "Scalar.DateTime" "createdAt" [] (Object.scalarDecoder |> Decode.map Github.Scalar.DateTime)


{-| Check if this comment was created via an email reply.
-}
createdViaEmail : SelectionSet Bool Github.Object.IssueComment
createdViaEmail =
    Object.selectionForField "Bool" "createdViaEmail" [] Decode.bool


{-| Identifies the primary key from the database.
-}
databaseId : SelectionSet (Maybe Int) Github.Object.IssueComment
databaseId =
    Object.selectionForField "(Maybe Int)" "databaseId" [] (Decode.int |> Decode.nullable)


{-| The actor who edited the comment.
-}
editor : SelectionSet decodesTo Github.Interface.Actor -> SelectionSet (Maybe decodesTo) Github.Object.IssueComment
editor object_ =
    Object.selectionForCompositeField "editor" [] object_ (identity >> Decode.nullable)


id : SelectionSet Github.Scalar.Id Github.Object.IssueComment
id =
    Object.selectionForField "Scalar.Id" "id" [] (Object.scalarDecoder |> Decode.map Github.Scalar.Id)


{-| Identifies the issue associated with the comment.
-}
issue : SelectionSet decodesTo Github.Object.Issue -> SelectionSet decodesTo Github.Object.IssueComment
issue object_ =
    Object.selectionForCompositeField "issue" [] object_ identity


{-| The moment the editor made the last edit
-}
lastEditedAt : SelectionSet (Maybe Github.Scalar.DateTime) Github.Object.IssueComment
lastEditedAt =
    Object.selectionForField "(Maybe Scalar.DateTime)" "lastEditedAt" [] (Object.scalarDecoder |> Decode.map Github.Scalar.DateTime |> Decode.nullable)


{-| Identifies when the comment was published at.
-}
publishedAt : SelectionSet (Maybe Github.Scalar.DateTime) Github.Object.IssueComment
publishedAt =
    Object.selectionForField "(Maybe Scalar.DateTime)" "publishedAt" [] (Object.scalarDecoder |> Decode.map Github.Scalar.DateTime |> Decode.nullable)


{-| Returns the pull request associated with the comment, if this comment was made on a
pull request.
-}
pullRequest : SelectionSet decodesTo Github.Object.PullRequest -> SelectionSet (Maybe decodesTo) Github.Object.IssueComment
pullRequest object_ =
    Object.selectionForCompositeField "pullRequest" [] object_ (identity >> Decode.nullable)


{-| A list of reactions grouped by content left on the subject.
-}
reactionGroups : SelectionSet decodesTo Github.Object.ReactionGroup -> SelectionSet (Maybe (List decodesTo)) Github.Object.IssueComment
reactionGroups object_ =
    Object.selectionForCompositeField "reactionGroups" [] object_ (identity >> Decode.list >> Decode.nullable)


type alias ReactionsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , content : OptionalArgument Github.Enum.ReactionContent.ReactionContent
    , orderBy : OptionalArgument Github.InputObject.ReactionOrder
    }


{-| A list of Reactions left on the Issue.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.
  - content - Allows filtering Reactions by emoji.
  - orderBy - Allows specifying the order in which reactions are returned.

-}
reactions : (ReactionsOptionalArguments -> ReactionsOptionalArguments) -> SelectionSet decodesTo Github.Object.ReactionConnection -> SelectionSet decodesTo Github.Object.IssueComment
reactions fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { first = Absent, after = Absent, last = Absent, before = Absent, content = Absent, orderBy = Absent }

        optionalArgs =
            [ Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "content" filledInOptionals.content (Encode.enum Github.Enum.ReactionContent.toString), Argument.optional "orderBy" filledInOptionals.orderBy Github.InputObject.encodeReactionOrder ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "reactions" optionalArgs object_ identity


{-| The repository associated with this node.
-}
repository : SelectionSet decodesTo Github.Object.Repository -> SelectionSet decodesTo Github.Object.IssueComment
repository object_ =
    Object.selectionForCompositeField "repository" [] object_ identity


{-| The HTTP path for this issue comment
-}
resourcePath : SelectionSet Github.Scalar.Uri Github.Object.IssueComment
resourcePath =
    Object.selectionForField "Scalar.Uri" "resourcePath" [] (Object.scalarDecoder |> Decode.map Github.Scalar.Uri)


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Github.Scalar.DateTime Github.Object.IssueComment
updatedAt =
    Object.selectionForField "Scalar.DateTime" "updatedAt" [] (Object.scalarDecoder |> Decode.map Github.Scalar.DateTime)


{-| The HTTP URL for this issue comment
-}
url : SelectionSet Github.Scalar.Uri Github.Object.IssueComment
url =
    Object.selectionForField "Scalar.Uri" "url" [] (Object.scalarDecoder |> Decode.map Github.Scalar.Uri)


type alias UserContentEditsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list of edits to this content.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
userContentEdits : (UserContentEditsOptionalArguments -> UserContentEditsOptionalArguments) -> SelectionSet decodesTo Github.Object.UserContentEditConnection -> SelectionSet (Maybe decodesTo) Github.Object.IssueComment
userContentEdits fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs =
            [ Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "before" filledInOptionals.before Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "userContentEdits" optionalArgs object_ (identity >> Decode.nullable)


{-| Check if the current viewer can delete this object.
-}
viewerCanDelete : SelectionSet Bool Github.Object.IssueComment
viewerCanDelete =
    Object.selectionForField "Bool" "viewerCanDelete" [] Decode.bool


{-| Can user react to this subject
-}
viewerCanReact : SelectionSet Bool Github.Object.IssueComment
viewerCanReact =
    Object.selectionForField "Bool" "viewerCanReact" [] Decode.bool


{-| Check if the current viewer can update this object.
-}
viewerCanUpdate : SelectionSet Bool Github.Object.IssueComment
viewerCanUpdate =
    Object.selectionForField "Bool" "viewerCanUpdate" [] Decode.bool


{-| Reasons why the current viewer can not update this comment.
-}
viewerCannotUpdateReasons : SelectionSet (List Github.Enum.CommentCannotUpdateReason.CommentCannotUpdateReason) Github.Object.IssueComment
viewerCannotUpdateReasons =
    Object.selectionForField "(List Enum.CommentCannotUpdateReason.CommentCannotUpdateReason)" "viewerCannotUpdateReasons" [] (Github.Enum.CommentCannotUpdateReason.decoder |> Decode.list)


{-| Did the viewer author this comment.
-}
viewerDidAuthor : SelectionSet Bool Github.Object.IssueComment
viewerDidAuthor =
    Object.selectionForField "Bool" "viewerDidAuthor" [] Decode.bool