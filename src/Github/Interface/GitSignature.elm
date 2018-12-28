-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Interface.GitSignature exposing (Fragments, email, fragments, isValid, maybeFragments, payload, signature, signer, state)

import Github.Enum.GitSignatureState
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
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Json.Decode as Decode


type alias Fragments decodesTo =
    { onGpgSignature : SelectionSet decodesTo Github.Object.GpgSignature
    , onSmimeSignature : SelectionSet decodesTo Github.Object.SmimeSignature
    , onUnknownSignature : SelectionSet decodesTo Github.Object.UnknownSignature
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Github.Interface.GitSignature
fragments selections =
    Object.exhuastiveFragmentSelection
        [ Object.buildFragment "GpgSignature" selections.onGpgSignature
        , Object.buildFragment "SmimeSignature" selections.onSmimeSignature
        , Object.buildFragment "UnknownSignature" selections.onUnknownSignature
        ]


{-| Can be used to create a non-exhuastive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onGpgSignature = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onSmimeSignature = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onUnknownSignature = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }


{-| Email used to sign this object.
-}
email : SelectionSet String Github.Interface.GitSignature
email =
    Object.selectionForField "String" "email" [] Decode.string


{-| True if the signature is valid and verified by GitHub.
-}
isValid : SelectionSet Bool Github.Interface.GitSignature
isValid =
    Object.selectionForField "Bool" "isValid" [] Decode.bool


{-| Payload for GPG signing object. Raw ODB object without the signature header.
-}
payload : SelectionSet String Github.Interface.GitSignature
payload =
    Object.selectionForField "String" "payload" [] Decode.string


{-| ASCII-armored signature header from object.
-}
signature : SelectionSet String Github.Interface.GitSignature
signature =
    Object.selectionForField "String" "signature" [] Decode.string


{-| GitHub user corresponding to the email signing this commit.
-}
signer : SelectionSet decodesTo Github.Object.User -> SelectionSet (Maybe decodesTo) Github.Interface.GitSignature
signer object_ =
    Object.selectionForCompositeField "signer" [] object_ (identity >> Decode.nullable)


{-| The state of this signature. `VALID` if signature is valid and verified by GitHub, otherwise represents reason why signature is considered invalid.
-}
state : SelectionSet Github.Enum.GitSignatureState.GitSignatureState Github.Interface.GitSignature
state =
    Object.selectionForField "Enum.GitSignatureState.GitSignatureState" "state" [] Github.Enum.GitSignatureState.decoder
