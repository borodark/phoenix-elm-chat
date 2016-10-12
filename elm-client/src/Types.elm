module Types exposing (..)


type alias User =
    { name : String
    }

type OpinionAboutPost
    = Love
    | Like
    | DontLike
    
-- Love, ["alice", "bob", User... ]
type alias Opinions =
    (OpinionAboutPost
    , List  
    )


type alias Message =
    {
      id : Int
    , user : String
    , body : String
--    , opinions : List Opinions
    }
