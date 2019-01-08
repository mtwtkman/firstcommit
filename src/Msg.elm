module Msg exposing (Msg(..))

import GraphQL.Client.Http as GraphQLClient
import Type exposing (..)


type alias InitialCommitResponse =
    Result GraphQLClient.Error (Maybe (List InitialCommit))


type alias CommitSummaryResponse =
    Result GraphQLClient.Error (Maybe CommitSummary)


type Msg
    = UpdateOwner String
    | UpdateName String
    | UpdateBranch String
    | UpdateApiToken String
    | SendCommitSummaryRequest
    | ReceiveCommitSummaryResponse CommitSummaryResponse
    | ReceiveInitialCommitResponse InitialCommitResponse
