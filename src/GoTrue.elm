module GoTrue exposing
    ( acceptInvite, acceptInviteExternalUrl, confirm, defaultConfig, login, signup
    , recover, requestPasswordRecovery
    , Config, DefaultHeaders, GoTrueResponse
    , PasswordRecoveryResponse, Token, User, UserUpdateAttributes
    , goTrueEncoder, userEncoder
    , goTrueDecoder, userDecoder, tokenDecoder, passwordRecoveryResponseDecoder
    , errorToString, getExpiry, refreshToken, responseToString, userToString
    , ExternalProvider(..), jwt, update
    )

{-| GoTrue-Elm : This library is an Elm port of the Netlify GoTrue-JS library


# GoTrue API

@docs acceptInvite, acceptInviteExternalUrl, confirm, defaultConfig, login, signup
@docs recover, requestPasswordRecovery, updateUser


# Types

@docs Config, DefaultHeaders, ExternalProvider, GoTrueResponse
@docs PasswordRecoveryResponse, Token, User, UserUpdateAttributes


# Encoders/Decoders

@docs goTrueEncoder, userEncoder
@docs goTrueDecoder, userDecoder, tokenDecoder, passwordRecoveryResponseDecoder


# Utility Functions

@docs errorToString, getExpiry, refreshToken, responseToString, userToString

-}

import Base64 as Base64
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra
import Result exposing (Result)
import Task exposing (Task)
import Url exposing (percentEncode)



--- PUBLIC API


{-| Create a new user with the specified configuration, email, and password
`GoTrue.signup GoTrue.defaultConfig "email@mydomain.com" "c!Pj6rMnNaaSSuM&"`
-}
signup : Config -> String -> String -> Task Http.Error GoTrueResponse
signup config email password =
    let
        jsonBody =
            Http.jsonBody (emailAndPasswordEncoder email password)

        rememberHeaders =
            if config.setCookie then
                getRememberHeaders config.setCookie

            else
                []

        url =
            config.goTrueUrl ++ "/signup"
    in
    Http.task
        { url = url
        , method = "POST"
        , body = jsonBody
        , headers = config.defaultHeaders ++ rememberHeaders
        , resolver = Http.stringResolver <| handleJsonResponse <| goTrueDecoder
        , timeout = Nothing
        }


{-| Confirm a user sign up with the specified configuration and token
`GoTrue.confirm GoTrue.defaultConfig "RECOVERY_TOKEN" (Result Http.Error GoTrueResponse -> msg)`
-}
confirm : Config -> String -> Task Http.Error GoTrueResponse
confirm config signup_token =
    verify config Signup signup_token


{-| Create a new user with the specified configuration, email, and password
`GoTrue.login GoTrue.defaultConfig "email@mydomain.com" "c!Pj6rMnNaaSSuM&"`
-}
login : Config -> String -> String -> Task Http.Error User
login config email password =
    maybeLogin config email password
        |> Task.andThen
            (\auth_token -> createUser config auth_token)


{-| Request a password recovery email with the specified configuration, email, and constructor
`GoTrue.requestPasswordRecovery GoTrue.defaultConfig "email@mydomain.com" (Result Http.Error PasswordRecoveryResponse -> msg)`
-}
requestPasswordRecovery : Config -> String -> Task Http.Error PasswordRecoveryResponse
requestPasswordRecovery config email =
    Http.task
        { url = config.goTrueUrl ++ "/recover"
        , method = "POST"
        , body = Http.jsonBody (emailEncoder email)
        , headers = config.defaultHeaders
        , resolver = Http.stringResolver <| handleJsonResponse <| passwordRecoveryResponseDecoder
        , timeout = Nothing
        }


{-| Accept a GoTrue invitation via the specified configuration and token
`GoTrue.recover GoTrue.defaultConfig "AUTH_TOKEN"`
-}
recover : Config -> String -> Task Http.Error GoTrueResponse
recover config recovery_token =
    verify config Recovery recovery_token


{-| Accept a GoTrue invitation via the specified configuration and token
`GoTrue.acceptInvite GoTrue.defaultConfig "INVITE_TOKEN"`
-}
acceptInvite : Config -> String -> String -> Task Http.Error User
acceptInvite config invite_token password =
    maybeVerify config invite_token password
        |> Task.andThen
            (\auth_token -> createUser config auth_token)


{-| Accept a GoTrue invitation from external provider via the specified configuration, provider, and token
Supported External Providers - Google, Bitbucket, Github, GitLib
`GoTrue.acceptInviteExternalUrl GoTrue.defaultConfig GoTrue.Provider "INVITE_TOKEN"`
-}
acceptInviteExternalUrl : Config -> ExternalProvider -> String -> Task Http.Error User
acceptInviteExternalUrl config externalProvider invite_token =
    let
        rememberHeaders =
            if config.setCookie then
                getRememberHeaders config.setCookie

            else
                []

        provider =
            case externalProvider of
                Bitbucket ->
                    "bitbucket"

                Github ->
                    "github"

                GitLab ->
                    "gitlab"

                Google ->
                    "google"

        body =
            "provider=" ++ provider ++ "&invite_token=" ++ percentEncode invite_token
    in
    Http.task
        { method = "POST"
        , url = config.goTrueUrl ++ "authorize"
        , headers = config.defaultHeaders ++ rememberHeaders
        , body = Http.stringBody "application/x-www-form-urlencoded" body
        , resolver = Http.stringResolver <| handleJsonResponse <| userDecoder
        , timeout = Nothing
        }


{-| Default library configuration
`{ goTrueUrl = "http://localhost:8888", apiUrl="/.netlify/identity", audience=Nothing, setCookie=False, defaultHeaders=[], tokenExpireyMargin = (60 * 1000), storageKey = "gotrue-elm.session" }`
-}
defaultConfig : Config
defaultConfig =
    { apiUrl = "/.netlify/identity"
    , audience = Nothing
    , defaultHeaders = []
    , goTrueUrl = "http://localhost:8888"
    , setCookie = False
    , storageKey = "gotrue-elm.session"
    , tokenExpireyMargin = 60 * 1000
    }


refreshToken : Maybe Encode.Value -> Maybe User
refreshToken user =
    user
        |> Maybe.andThen
            (\u ->
                case Decode.decodeValue userDecoder u of
                    Ok result ->
                        let
                            token =
                                result.token

                            claims =
                                Maybe.withDefault 0 (getExpiry token)

                            freshToken =
                                { token | expiresAt = Just (claims * 1000) }

                            newUser =
                                { result | token = freshToken }
                        in
                        Just newUser

                    Err _ ->
                        Nothing
            )


update : Config -> Token -> UserUpdateAttributes -> Task Http.Error GoTrueResponse
update config token attrs =
    let
        tokenHeader =
            Http.header "Authorization" ("Bearer " ++ token.accessToken)

        audHeader =
            case config.audience of
                Just aud ->
                    [ Http.header "X-JWT-AUD" aud ]

                Nothing ->
                    []
    in
    Http.task
        { url = config.goTrueUrl ++ "/user"
        , method = "PUT"
        , body = Http.jsonBody (userUpdateAttributesEncoder attrs)
        , headers = tokenHeader :: audHeader
        , resolver = Http.stringResolver <| handleJsonResponse <| goTrueDecoder
        , timeout = Nothing
        }


-- forbiddenUpdateAttributes : List String
-- forbiddenUpdateAttributes =
--     [ "API"
--     , "TOKEN"
--     , "AUDIENCE"
--     , "URL"
--     ]
-- forbiddenSaveAttributes : List String
-- forbiddenSaveAttributes =
--     [ "API" ]


jwt : Maybe User -> Result String String
jwt user =
    case user of
        Nothing ->
            Err "No User"

        Just u ->
            Ok u.token.accessToken


{-|

@docs AppMetadata

-}
type alias AppMetadata =
    { provider : String
    , roles : List String
    }


{-|

@docs Configuration

-}
type alias Config =
    { apiUrl : String
    , audience : Maybe String
    , defaultHeaders : List Http.Header
    , goTrueUrl : String
    , setCookie : Bool
    , storageKey : String
    , tokenExpireyMargin : Int
    }


{-|

@docs DefaultHeaders

-}
type alias DefaultHeaders =
    { xUseCookie : String
    , xGoTrueToken : String
    }


{-|

@docs External Provider

-}
type ExternalProvider
    = Bitbucket
    | Github
    | GitLab
    | Google


{-|

@docs GoTrueResponse

-}
type alias GoTrueResponse =
    { app_metadata : Maybe AppMetadata
    , aud : String
    , confirmation_sent_at : Maybe String
    , confirmed_at : Maybe String
    , created_at : String
    , email : String
    , id : String
    , recovery_sent_at : Maybe String
    , role : String
    , updated_at : String
    , user_metadata : Maybe UserMetadata
    }


{-|

@docs PasswordRecoveryResponse

-}
type alias PasswordRecoveryResponse =
    {}


{-|

@docs Token

-}
type alias Token =
    { accessToken : String
    , expiresAt : Maybe Int
    , expiresIn : Int
    , refreshToken : String
    , tokenType : String
    }


{-|

@docs User

-}
type alias User =
    { app_metadata : Maybe AppMetadata
    , aud : String
    , confirmation_sent_at : Maybe String
    , confirmed_at : Maybe String
    , created_at : String
    , email : String
    , id : String
    , recovery_sent_at : Maybe String
    , role : String
    , token : Token
    , updated_at : String
    , url : String
    , user_metadata : Maybe UserMetadata
    }


{-|

@docs UserMetadata

-}
type alias UserMetadata =
    { email : Maybe String
    , fullName : Maybe String
    }


{-|

@docs VerifyType

-}
type VerifyType
    = Recovery
    | Signup


{-|

@docs VerifyType

-}
type alias UserUpdateAttributes =
    { email : Maybe String
    , password : Maybe String
    , emailChangeToken : Maybe String
    , appMetadata : Maybe AppMetadata
    , userMetadata : Maybe UserMetadata
    }



--- ENCODERS/DECODERS


appMetadataDecoder : Decoder AppMetadata
appMetadataDecoder =
    Decode.succeed AppMetadata
        |> required "provider" Decode.string
        |> optional "roles" (Decode.list Decode.string) []


appMetadataEncoder : AppMetadata -> Encode.Value
appMetadataEncoder data =
    Encode.object
        [ ( "provider", Encode.string data.provider ) ]


userUpdateAttributesEncoder : UserUpdateAttributes -> Encode.Value
userUpdateAttributesEncoder data =
    Encode.object
        [ ( "email", EncodeExtra.maybe Encode.string data.email )
        , ( "password", EncodeExtra.maybe Encode.string data.password )
        , ( "email_change_token", EncodeExtra.maybe Encode.string data.emailChangeToken )
        , ( "app_metadata", EncodeExtra.maybe appMetadataEncoder data.appMetadata )
        , ( "data", EncodeExtra.maybe userMetadataEncoder data.userMetadata )
        ]


emailEncoder : String -> Encode.Value
emailEncoder email =
    Encode.object [ ( "email", Encode.string email ) ]


emailAndPasswordEncoder : String -> String -> Encode.Value
emailAndPasswordEncoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


emptyObjectDecoder : a -> Decoder a
emptyObjectDecoder default =
    Decode.keyValuePairs (Decode.succeed default)
        |> Decode.andThen
            (\pairs ->
                case pairs of
                    [] ->
                        Decode.succeed default

                    _ ->
                        Decode.fail "Expecting an empty object"
            )


goTrueEncoder : GoTrueResponse -> Encode.Value
goTrueEncoder data =
    Encode.object
        [ ( "id", Encode.string data.id )
        , ( "aud", Encode.string data.aud )
        , ( "role", Encode.string data.role )
        , ( "email", Encode.string data.email )
        , ( "confirmation_sent_at", EncodeExtra.maybe Encode.string data.confirmation_sent_at )
        , ( "app_metadata", EncodeExtra.maybe appMetadataEncoder data.app_metadata )
        , ( "user_metadata", EncodeExtra.maybe userMetadataEncoder data.user_metadata )
        , ( "created_at", Encode.string data.created_at )
        , ( "updated_at", Encode.string data.updated_at )
        ]


goTrueDecoder : Decoder GoTrueResponse
goTrueDecoder =
    Decode.succeed GoTrueResponse
        |> required "app_metadata" (Decode.nullable appMetadataDecoder)
        |> required "aud" Decode.string
        |> optional "confirmed_at" (Decode.nullable Decode.string) (Just "")
        |> optional "confirmation_sent_at" (Decode.nullable Decode.string) (Just "")
        |> required "created_at" Decode.string
        |> required "email" Decode.string
        |> required "id" Decode.string
        |> optional "recovery_sent_at" (Decode.nullable Decode.string) (Just "")
        |> required "role" Decode.string
        |> required "updated_at" Decode.string
        |> optional "user_metadata" (Decode.map Just (Decode.oneOf [ emptyObjectDecoder (UserMetadata Nothing Nothing), userMetadataDecoder ])) Nothing


goTrueResponseEncoder : GoTrueResponse -> Encode.Value
goTrueResponseEncoder data =
    Encode.object
        [ ( "id", Encode.string data.id )
        , ( "aud", Encode.string data.aud )
        , ( "role", Encode.string data.role )
        , ( "email", Encode.string data.email )
        , ( "confirmation_sent_at", EncodeExtra.maybe Encode.string data.confirmation_sent_at )
        , ( "app_metadata", EncodeExtra.maybe appMetadataEncoder data.app_metadata )
        , ( "user_metadata", EncodeExtra.maybe userMetadataEncoder data.user_metadata )
        , ( "created_at", Encode.string data.created_at )
        , ( "updated_at", Encode.string data.updated_at )
        ]


passwordRecoveryResponseDecoder : Decoder PasswordRecoveryResponse
passwordRecoveryResponseDecoder =
    Decode.succeed PasswordRecoveryResponse


tokenDecoder : Decoder Token
tokenDecoder =
    Decode.succeed Token
        |> required "access_token" Decode.string
        |> optional "expires_at" (Decode.nullable Decode.int) (Just 0)
        |> required "expires_in" Decode.int
        |> required "refresh_token" Decode.string
        |> required "token_type" Decode.string


tokenEncoder : Token -> Encode.Value
tokenEncoder token =
    Encode.object
        [ ( "access_token", Encode.string token.accessToken )
        , ( "token_type", Encode.string token.tokenType )
        , ( "expires_in", Encode.int token.expiresIn )
        , ( "refresh_token", Encode.string token.refreshToken )
        , ( "expires_at", EncodeExtra.maybe Encode.int token.expiresAt )
        ]


tokenAndTypeEncoder : String -> VerifyType -> Encode.Value
tokenAndTypeEncoder token verifyType =
    let
        tokenType =
            case verifyType of
                Signup ->
                    "signup"

                Recovery ->
                    "recovery"
    in
    Encode.object
        [ ( "token", Encode.string token )
        , ( "type", Encode.string tokenType )
        ]


tokenPwdAndTypeEncoder : String -> String -> VerifyType -> Encode.Value
tokenPwdAndTypeEncoder token password verifyType =
    let
        tokenType =
            case verifyType of
                Signup ->
                    "signup"

                Recovery ->
                    "recovery"
    in
    Encode.object
        [ ( "token", Encode.string token )
        , ( "password", Encode.string password )
        , ( "type", Encode.string tokenType )
        ]


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "app_metadata" (Decode.nullable appMetadataDecoder)
        |> required "aud" Decode.string
        |> optional "confirmation_sent_at" (Decode.nullable Decode.string) (Just "")
        |> optional "confirmed_at" (Decode.nullable Decode.string) (Just "")
        |> required "created_at" Decode.string
        |> required "email" Decode.string
        |> required "id" Decode.string
        |> optional "recovery_sent_at" (Decode.nullable Decode.string) (Just "")
        |> required "role" Decode.string
        |> required "token" tokenDecoder
        |> required "updated_at" Decode.string
        |> required "api_url" Decode.string
        |> optional "user_metadata" (Decode.map Just (Decode.oneOf [ emptyObjectDecoder (UserMetadata Nothing Nothing), userMetadataDecoder ])) Nothing


userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ( "token", tokenEncoder user.token )
        , ( "api_url", Encode.string user.url )
        , ( "id", Encode.string user.id )
        , ( "aud", Encode.string user.aud )
        , ( "role", Encode.string user.role )
        , ( "email", Encode.string user.email )
        , ( "confirmed_at", EncodeExtra.maybe Encode.string user.confirmed_at )
        , ( "confirmation_sent_at", EncodeExtra.maybe Encode.string user.confirmation_sent_at )
        , ( "recovery_sent_at", EncodeExtra.maybe Encode.string user.recovery_sent_at )
        , ( "app_metadata", EncodeExtra.maybe appMetadataEncoder user.app_metadata )
        , ( "user_metadata", EncodeExtra.maybe userMetadataEncoder user.user_metadata )
        , ( "created_at", Encode.string user.created_at )
        , ( "updated_at", Encode.string user.updated_at )
        ]


userMetadataDecoder : Decoder UserMetadata
userMetadataDecoder =
    Decode.succeed UserMetadata
        |> optional "email" (Decode.nullable Decode.string) (Just "null")
        |> optional "full_name" (Decode.nullable Decode.string) (Just "null")


userMetadataEncoder : UserMetadata -> Encode.Value
userMetadataEncoder data =
    Encode.object
        [ ( "email", EncodeExtra.maybe Encode.string data.email )
        , ( "full_name", EncodeExtra.maybe Encode.string data.fullName )
        ]


userToString : Maybe User -> Maybe String
userToString user =
    user |> Maybe.andThen (\u -> Just (Encode.encode 4 (userEncoder u)))


responseToString : Maybe GoTrueResponse -> Maybe String
responseToString response =
    response |> Maybe.andThen (\r -> Just (Encode.encode 4 (goTrueResponseEncoder r)))


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



--- PRIVATE FUNCTIONS


type alias Exp =
    { exp : Int }


createUser : Config -> Token -> Task Http.Error User
createUser config token =
    let
        tokenHeader =
            Http.header "Authorization" ("Bearer " ++ token.accessToken)

        audHeader =
            case config.audience of
                Just aud ->
                    [ Http.header "X-JWT-AUD" aud ]

                Nothing ->
                    []

        url =
            config.goTrueUrl ++ "/user"
    in
    Http.task
        { url = url
        , method = "GET"
        , headers = tokenHeader :: audHeader
        , body = Http.emptyBody
        , resolver = Http.stringResolver (userResponse config token goTrueDecoder)
        , timeout = Nothing
        }


decodeExpFromClaim : Decoder Exp
decodeExpFromClaim =
    Decode.succeed Exp
        |> required "exp" Decode.int


getExpiry : Token -> Maybe Int
getExpiry token =
    let
        claims =
            Maybe.withDefault "0" <| List.head <| List.drop 1 <| String.split "." token.accessToken

        decodeResult =
            Result.withDefault "" (Base64.decode claims)

        expire =
            case Decode.decodeString decodeExpFromClaim decodeResult of
                Ok expire_result ->
                    Just expire_result.exp

                Err _ ->
                    Nothing
    in
    expire


getRememberHeaders : Bool -> List Http.Header
getRememberHeaders remember =
    let
        r =
            if remember then
                "1"

            else
                "session"
    in
    [ Http.header "X-Use-Cookie" r ]


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Ok result ->
                    Ok result

                Err _ ->
                    Err (Http.BadBody body)


maybeLogin : Config -> String -> String -> Task Http.Error Token
maybeLogin config email password =
    let
        url =
            config.goTrueUrl ++ "/token"

        body =
            "grant_type=password"
                ++ "&username="
                ++ percentEncode email
                ++ "&password="
                ++ percentEncode password

        rememberHeaders =
            getRememberHeaders config.setCookie
    in
    Http.task
        { url = url
        , method = "POST"
        , body = Http.stringBody "application/x-www-form-urlencoded" body
        , headers = config.defaultHeaders ++ rememberHeaders
        , resolver = Http.stringResolver <| handleJsonResponse <| tokenDecoder
        , timeout = Nothing
        }


maybeVerify : Config -> String -> String -> Task Http.Error Token
maybeVerify config token password =
    let
        rememberHeaders =
            if config.setCookie then
                getRememberHeaders config.setCookie

            else
                []

        url =
            config.goTrueUrl ++ "/verify"

        jsonBody =
            Http.jsonBody (tokenPwdAndTypeEncoder token password Signup)
    in
    Http.task
        { url = url
        , method = "POST"
        , body = jsonBody
        , headers = config.defaultHeaders ++ rememberHeaders
        , resolver = Http.stringResolver <| handleJsonResponse <| tokenDecoder
        , timeout = Nothing
        }


userResponse : Config -> Token -> Decoder GoTrueResponse -> Http.Response String -> Result Http.Error User
userResponse config token decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Ok u ->
                    Ok (User u.app_metadata u.aud u.confirmation_sent_at u.confirmed_at u.created_at u.email u.id u.recovery_sent_at u.role token u.updated_at config.apiUrl u.user_metadata)

                Err _ ->
                    Err (Http.BadBody body)


verify : Config -> VerifyType -> String -> Task Http.Error GoTrueResponse
verify config verifyType token =
    let
        rememberHeaders =
            if config.setCookie then
                getRememberHeaders config.setCookie

            else
                []
    in
    Http.task
        { method = "POST"
        , url = config.goTrueUrl ++ "/verify"
        , headers = config.defaultHeaders ++ rememberHeaders
        , body = Http.jsonBody (tokenAndTypeEncoder token verifyType)
        , resolver = Http.stringResolver <| handleJsonResponse <| goTrueDecoder
        , timeout = Nothing
        }
