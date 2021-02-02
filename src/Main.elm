module Main exposing (..)

import GoTrue
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url



---- PROGRAM ----


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


---- MODEL ----


type alias Model =
    { config : GoTrue.Config
    , key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  let
      config = GoTrue.defaultConfig
      goTrueUrl = "https://" ++ url.host ++ config.apiUrl
      newConfig = {config | goTrueUrl = goTrueUrl}
  in
    ( Model newConfig key url
    , Cmd.none
    )
   

  
type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
   

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
        let
            config = model.config
            goTrueUrl = "https://" ++ url.host ++ model.config.apiUrl
            newConfig = {config | goTrueUrl = goTrueUrl}
        in
        
      ( { model | url = url, config = newConfig }
      , Cmd.none
      )


view : Model -> Browser.Document Msg
view model =
  { title = "URL Interceptor"
  , body =
      [ text "The current URL is: "
      , a [ href "/" ] [text "home"]
      , b [] [ text (model.config.goTrueUrl) ]
      ]
  }