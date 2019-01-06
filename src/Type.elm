module Type exposing (CommitSummary, InitialCommit, Model)


type alias CommitSummary =
    { cursor : List String
    , totalCount : Int
    }


type alias InitialCommit =
    { commitUrl : String
    , message : String
    }


type alias Model =
    { owner : String
    , name : String
    , qualifiedName : String
    , apiToken : String
    , initialCommit : Maybe InitialCommit
    , fetching : Bool
    , errorMessage : String
    }
