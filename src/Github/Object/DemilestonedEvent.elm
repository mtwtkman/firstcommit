-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.DemilestonedEvent exposing (actor, createdAt, id, milestoneTitle, subject)

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


{-| Identifies the actor who performed the event.
-}
actor : SelectionSet decodesTo Github.Interface.Actor -> SelectionSet (Maybe decodesTo) Github.Object.DemilestonedEvent
actor object_ =
    Object.selectionForCompositeField "actor" [] object_ (identity >> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Github.Scalar.DateTime Github.Object.DemilestonedEvent
createdAt =
    Object.selectionForField "Scalar.DateTime" "createdAt" [] (Object.scalarDecoder |> Decode.map Github.Scalar.DateTime)


id : SelectionSet Github.Scalar.Id Github.Object.DemilestonedEvent
id =
    Object.selectionForField "Scalar.Id" "id" [] (Object.scalarDecoder |> Decode.map Github.Scalar.Id)


{-| Identifies the milestone title associated with the 'demilestoned' event.
-}
milestoneTitle : SelectionSet String Github.Object.DemilestonedEvent
milestoneTitle =
    Object.selectionForField "String" "milestoneTitle" [] Decode.string


{-| Object referenced by event.
-}
subject : SelectionSet decodesTo Github.Union.MilestoneItem -> SelectionSet decodesTo Github.Object.DemilestonedEvent
subject object_ =
    Object.selectionForCompositeField "subject" [] object_ identity