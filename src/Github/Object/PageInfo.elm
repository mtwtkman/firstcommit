-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.PageInfo exposing (endCursor, hasNextPage, hasPreviousPage, startCursor)

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


{-| When paginating forwards, the cursor to continue.
-}
endCursor : SelectionSet (Maybe String) Github.Object.PageInfo
endCursor =
    Object.selectionForField "(Maybe String)" "endCursor" [] (Decode.string |> Decode.nullable)


{-| When paginating forwards, are there more items?
-}
hasNextPage : SelectionSet Bool Github.Object.PageInfo
hasNextPage =
    Object.selectionForField "Bool" "hasNextPage" [] Decode.bool


{-| When paginating backwards, are there more items?
-}
hasPreviousPage : SelectionSet Bool Github.Object.PageInfo
hasPreviousPage =
    Object.selectionForField "Bool" "hasPreviousPage" [] Decode.bool


{-| When paginating backwards, the cursor to continue.
-}
startCursor : SelectionSet (Maybe String) Github.Object.PageInfo
startCursor =
    Object.selectionForField "(Maybe String)" "startCursor" [] (Decode.string |> Decode.nullable)
