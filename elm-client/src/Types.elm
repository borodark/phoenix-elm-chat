module Types exposing (..)


type alias User =
    { name : String
    }


type alias Message =
    { -- id : Int
      --,
      user : String
    , body : String
    }



-- ++, +, -


type OpinionAboutPost
    = Love
    | Like
    | DontLike
