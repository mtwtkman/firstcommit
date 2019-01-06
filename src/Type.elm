module Type exposing (CommitSummary, Fetching(..), Form, InitialCommit, Model)


type alias CommitSummary =
    { cursor : List String
    , totalCount : Int
    }


type alias InitialCommit =
    { commitUrl : String
    , message : String
    }


type alias Model =
    { form : Form
    , fetching : Fetching
    , sendable : Bool
    , initialCommit : Maybe InitialCommit
    }


type Fetching
    = NotFetching
    | FetchingInitialCommit
    | FetchingCommitSummary
    | Done


type alias Form =
    { owner : Maybe String
    , name : Maybe String
    , qualifiedName : Maybe String
    , apiToken : Maybe String
    }
