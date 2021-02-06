module Util exposing ( errorStyle
                     , errorToString
                     , goTrueResponseToString
                     , successStyle
                     , userToString
                     )

import GoTrue
import Http
import Json.Encode as Encode
import Html
import Html.Attributes exposing (style)


userToString : Maybe GoTrue.User -> Maybe String
userToString user =
    user |> Maybe.andThen (\u -> Just (Encode.encode 4 (GoTrue.userEncoder u)))


goTrueResponseToString : Maybe GoTrue.GoTrueResponse -> Maybe String
goTrueResponseToString signup =
    signup |> Maybe.andThen (\s -> Just (Encode.encode 4 (GoTrue.goTrueEncoder s)))


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage


successStyle : List (Html.Attribute msg)
successStyle =
    [ style "background-color" "LightGreen" ]


errorStyle : List (Html.Attribute msg)
errorStyle =
    [ style "background-color" "LightPink"
    , style "padding" "40px"
    ]
