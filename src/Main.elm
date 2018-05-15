module Main exposing (main)

import Css exposing (..)
import Css.Foreign exposing (body, each, global)
import Css.Helpers exposing (toCssIdentifier)
import Dict
import Html
import Html.Styled exposing (Html, button, div, fromUnstyled, li, styled, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (class, classList)
import Html.Styled.Events exposing (onClick)


bodyStyleNode : Html msg
bodyStyleNode =
    global
        [ body
            [ backgroundColor (hex "#fff")
            , color (hex "#000")
            , fontFamilies [ "sans-serif" ]
            , boxSizing borderBox
            , paddingTop (px 20)
            ]
        , each
            [ Css.Foreign.input, Css.Foreign.textarea, Css.Foreign.button ]
            [ property "-webkit-appearance" "none" --Safari/Chrome
            , property "-moz-appearance" "none" -- Firefox
            , property "-ms-appearance" "none" -- IE
            , property "-o-appearance" "none" -- Opera
            , property "appearance" "none"
            , property "-webkit-border-radius" "0"
            ]
        ]


type CssClasses
    = Div1Class
    | TilesClass
    | RowClass
    | TileClass
    | ButtonClass
    | DateBarClass


tileMarginPx : number
tileMarginPx =
    36


tilesStyleNode : Html msg
tilesStyleNode =
    Css.Foreign.global
        [ Css.Foreign.class TilesClass
            [ displayFlex
            , flexDirection column
            ]
        , Css.Foreign.class RowClass
            [ displayFlex
            , width (pct 100)
            ]
        , Css.Foreign.class TileClass
            [ width (pct 50)
            , height (px 200)
            , property "box-shadow" "0 3px 6px rgba(0,0,0,0.16), 0 3px 6px rgba(0,0,0,0.23)"
            , marginLeft (px tileMarginPx)
            , marginTop (px tileMarginPx)
            , lastChild
                [ marginRight (px tileMarginPx)
                ]
            , padding (px 20)
            , fontSize (px 34)
            ]
        , Css.Foreign.class ButtonClass
            [ margin (px 10)
            , width (px 200)
            , height (px 100)
            , fontSize (px 36)
            ]
        , Css.Foreign.class DateBarClass
            [ displayFlex
            , justifyContent center
            , alignItems center
            ]
        ]


renderRow : List String -> Html msg
renderRow row =
    row
        |> List.map
            (\tile ->
                div [ class (toCssIdentifier TileClass) ] [ text tile ]
            )
        |> div [ class (toCssIdentifier RowClass) ]


renderTiles : List (List String) -> Html msg
renderTiles tiles =
    tiles
        |> List.map renderRow
        |> (::) tilesStyleNode
        |> div [ class (toCssIdentifier TilesClass) ]


getLastDate : Dict.Dict String { tiles : List (List String) } -> String
getLastDate model =
    model
        |> Dict.keys
        |> lastElem
        |> Maybe.withDefault ""


lastElem : List a -> Maybe a
lastElem =
    List.foldl (Just >> always) Nothing


previousElem : String -> List String -> Maybe String
previousElem last list =
    list
        |> List.filter (\item -> item < last)
        |> List.head


nextElem : String -> List String -> Maybe String
nextElem last list =
    list
        |> List.filter (\item -> item > last)
        |> List.head


view : Model -> Html.Html Msg
view model =
    styled div
        []
        []
        [ bodyStyleNode
        , div [ class (toCssIdentifier DateBarClass) ]
            [ button [ onClick Previous, class (toCssIdentifier ButtonClass) ] [ text "Previous" ]
            , text model.selectedDate
            , button [ onClick Next, class (toCssIdentifier ButtonClass) ] [ text "Next" ]
            ]
        , renderTiles (model.calendar |> Dict.get model.selectedDate |> Maybe.withDefault { tiles = [] } |> .tiles)
        , div [] [ text <| toString model ]
        , text <| toString <| previousElem "2018-05-15" [ "2018-05-16", "2018-05-15", "2018-05-14", "2018-05-13" ]
        , text <| toString <| nextElem "2018-05-15" [ "2018-05-16", "2018-05-15", "2018-05-14", "2018-05-13" ]
        ]
        |> toUnstyled


type alias Model =
    { calendar : Dict.Dict String { tiles : List (List String) }
    , selectedDate : String
    }


tiles : List (List String)
tiles =
    [ [ "Meditation", "albert" ]
    , [ "Rust", "e-commerce" ]
    ]


tiles2 : List (List String)
tiles2 =
    [ [ "ELM", "AI" ]
    , [ "grasshopper app", "e-commerce" ]
    ]


calendar : Dict.Dict String { tiles : List (List String) }
calendar =
    Dict.fromList
        [ ( "2018-05-14", { tiles = tiles } )
        , ( "2018-05-15", { tiles = tiles2 } )
        ]


model : Model
model =
    Model calendar (getLastDate calendar)


type Msg
    = Previous
    | Next


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Previous ->
            ( { model | selectedDate = previousElem model.selectedDate (Dict.keys model.calendar) |> Maybe.withDefault model.selectedDate }, Cmd.none )

        Next ->
            ( { model | selectedDate = nextElem model.selectedDate (Dict.keys model.calendar) |> Maybe.withDefault model.selectedDate }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }
