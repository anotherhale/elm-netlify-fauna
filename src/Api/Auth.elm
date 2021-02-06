module Api.Auth exposing (..)

--/ This is an abstract Auth API and can have different implementations
--/ based on which Authentication package is being used
import GoTrue as GoTrue

type alias Model = 
    { config: GoTrue.Config
    , strategy: String
    }