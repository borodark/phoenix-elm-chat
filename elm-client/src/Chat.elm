module Chat exposing (view, initialModel, update, Model, Msg(..), OutMsg(..), encodeMessage)

import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class, type', name)
import Html.Events exposing (onInput, onClick, onSubmit)
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))
import Styles
import Types exposing (User, Message, OpinionAboutPost)
import Material
import Material.Card as Card
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Textfield as Textfield
import Material.List as List
import Markdown


type Msg
    = SetNewMessage String
    | ReceiveMessage JE.Value
    | ReceiveHistory JE.Value
    | SendMessage
    | Mdl (Material.Msg Msg)
    | Rate OpinionAboutPost Int


type OutMsg
    = Say String


type alias Model =
    { currentUser : String
    , newMessage : String
    , topic : String
    , messages : List Message
    , users : List User
    , mdl : Material.Model
    }


initialModel : Model
initialModel =
    { currentUser = "From Initial Model"
    , newMessage = ""
    , topic = ""
    , messages = []
    , users = []
    , mdl = Material.model
    }


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    let
       som = Debug.log "From update top -> User is:" model
    in
    case msg of
        SetNewMessage string ->
            ( { model | newMessage = string }
            , Cmd.none
            , Nothing
            )

        Rate aRating aMessageId ->
            let
                aCurrentUser = Debug.log "Rate from User -> : " model.currentUser
                aNewMessage =
                    Debug.log "Rate Message -> : " aMessageId
                aNewRating = Debug.log "Rated -> : " aRating
                -- TODO Perhaps find message in model.messages 
                -- TODO Send current user and message id to endpoint collecting likes
                --
            in
                ( { model | messages = model.messages }
                , Cmd.none
                , Nothing
                )

        SendMessage ->
            ( { model | newMessage = "" }
            , Cmd.none
            , Just <| Say model.newMessage
            )

        ReceiveMessage raw ->
            case JD.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    ( { model | messages = model.messages ++ [ chatMessage ] }
                    , Cmd.none
                    , Nothing
                    )

                Err error ->
                    ( model
                    , Cmd.none
                    , Nothing
                    )

        ReceiveHistory raw ->
            case JD.decodeValue chatHistoryDecoder raw of
                Ok chatMessages ->
                    ( { model | messages = model.messages ++ chatMessages }
                    , Cmd.none
                    , Nothing
                    )

                Err error ->
                    ( model
                    , Cmd.none
                    , Nothing
                    )

        Mdl msg' ->
            let
                ( newModel, newCmd ) =
                    Material.update msg' model
            in
                ( newModel
                , newCmd
                , Nothing
                )


view : Model -> Html Msg
view model =
    let
        aValue = Debug.log "From Chat.view User is:" model.currentUser
    in
    Card.view
        [ Options.css "width" "100%"
        , Elevation.e2
        ]
        [ Card.title
            [ Color.background Color.primary
            , Color.text Color.white
            ]
            [ Card.head [] [ text model.topic ] ]
        , Card.text
            []
            [ messageListView model
            , messageInputView model
            ]
        ]


messageListView : Model -> Html Msg
messageListView model =
    let
        aValue = Debug.log "From messageListView -> User is:" model.currentUser
    in

    List.ul
        []
        (List.map viewMessage model.messages)


viewMessage : Message -> Html Msg
viewMessage message =
    List.li
        [ List.withBody ]
        [ List.content
            []
            [ List.avatarImage ("https://api.adorable.io/avatars/285/" ++ message.user ++ ".png") []
            , text message.user
            , List.body
                []
                [ Markdown.toHtml [] message.body
                , div
                    [ Html.Attributes.style [ ( "display", "inline" ) ] ]
                    [ likeButtons ""
                        [ ( message, "Love" , Rate Types.Love message.id )
                        , ( message, "Like", Rate Types.Like message.id )
                        , ( message, "Noooo!", Rate Types.DontLike message.id )
                        ]
                    ]
                ]
            ]
        ]


likeButtons : String -> List ( Message, String, Msg ) -> Html Msg
likeButtons pickerClass options =
    fieldset [] (List.map like options)


like : ( Message, String, Msg ) -> Html Msg
like ( message, aName, msg ) =
    div [ class "signup-button", onClick msg ] [ text aName ]



--   label []
--     [ input [ type' "radio", onClick msg ] []
--   , text aName
-- ]


messageInputView : Model -> Html Msg
messageInputView model =
    form
        [ onSubmit SendMessage
        ]
        [ Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Textfield.onInput SetNewMessage
            , Textfield.value model.newMessage
            , Textfield.label "Type a message..."
            ]
        ]


userView : User -> Html Msg
userView user =
    li []
        [ text user.name
        ]


chatMessageDecoder : JD.Decoder Message
chatMessageDecoder =
    JD.object3 Message
        ("id" := JD.int)
        (JD.oneOf
            [ ("user" := JD.string)
            , JD.succeed "anonymous"
            ]
        )
        ("body" := JD.string)



-- ( oneOf "opinions" := Message Nothing )


chatHistoryDecoder : JD.Decoder (List Message)
chatHistoryDecoder =
    JD.object1 identity
        ("history" := (JD.list chatMessageDecoder))


encodeMessage : String -> JE.Value
encodeMessage message =
    (JE.object [ ( "body", JE.string message ) ])
