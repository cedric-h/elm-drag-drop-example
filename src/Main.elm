module Main exposing (main)

import Browser
import Browser.Events as BrowserEvents
import Html exposing (Html, button, div, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as D
import Json.Encode as E



-- VECTOR


type alias Vec2 =
    ( Float, Float )


vec2 : Float -> Float -> Vec2
vec2 x y =
    ( x, y )


sub : Vec2 -> Vec2 -> Vec2
sub ( a, b ) ( x, y ) =
    ( a - x, b - y )


add : Vec2 -> Vec2 -> Vec2
add ( a, b ) ( x, y ) =
    ( a + x, b + y )


coordToAttr : String -> Float -> Html.Attribute msg
coordToAttr attr coord =
    Attributes.style attr <|
        (\a -> String.append a "px") <|
            String.fromInt <|
                round coord


posAttr : Vec2 -> List (Html.Attribute msg)
posAttr ( x, y ) =
    [ coordToAttr "left" x
    , coordToAttr "top" y
    ]



-- MODEL


type alias Model =
    { pos : Vec2
    , grabbedAt : Maybe Vec2
    , screenSize : Vec2
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { pos = ( 50, 50 )
      , grabbedAt = Nothing
      , screenSize = ( 0, 0 )
      }
    , Cmd.none
    )



-- MOUSE MOVE


type alias MouseEventData =
    { relativePos : Vec2
    , screenPos : Vec2
    , targetSize : Vec2
    }


mouseEventDecoder : D.Decoder MouseEventData
mouseEventDecoder =
    D.map3 MouseEventData
        (D.map2 vec2
            (D.at [ "offsetX" ] D.float)
            (D.at [ "offsetY" ] D.float)
        )
        (D.map2 vec2
            (D.at [ "screenX" ] D.float)
            (D.at [ "screenY" ] D.float)
        )
        (D.map2 vec2
            (D.at [ "target", "offsetWidth" ] D.float)
            (D.at [ "target", "offsetHeight" ] D.float)
        )



-- UPDATE


type Msg
    = Grab MouseEventData
    | Drag MouseEventData
    | Drop
    | ScreenSize Vec2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Grab { screenPos } ->
            ( { model | grabbedAt = Just screenPos }
            , Cmd.none
            )

        Drag { screenPos } ->
            let
                newPos =
                    Maybe.withDefault
                        model.pos
                        (Maybe.map (add model.pos << sub screenPos) model.grabbedAt)
            in
            ( { model | pos = newPos, grabbedAt = Just screenPos }
            , Cmd.none
            )

        Drop ->
            ( { model | grabbedAt = Nothing }, Cmd.none )

        ScreenSize size ->
            ( { model | screenSize = size }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        (List.concat
            [ [ Attributes.id "elm" ]
            , case model.grabbedAt of
                Just pos ->
                    [ Events.on "mouseup" (D.succeed Drop)
                    , Events.on "mousemove" (D.map Drag mouseEventDecoder)
                    ]

                Nothing ->
                    []
            , [ coordToAttr "width" (Tuple.first model.screenSize)
              , coordToAttr "height" (Tuple.second model.screenSize)
              ]
            ]
        )
        [ div
            (List.concat
                [ [ Attributes.id "box"
                  , Attributes.class "noselect"
                  ]
                , case model.grabbedAt of
                    Just pos ->
                        []

                    Nothing ->
                        [ Events.on "mousedown" (D.map Grab mouseEventDecoder) ]
                , posAttr model.pos
                ]
            )
            [ text "hm"

            -- <| Maybe.withDefault "hm"
            -- <| Maybe.map (Tuple.first >> String.fromFloat) model.grabbedAt
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    BrowserEvents.onResize (\a b -> ScreenSize ( toFloat a, toFloat b ))



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
