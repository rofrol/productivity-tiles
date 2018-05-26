module Main exposing (main)

import Css exposing (..)
import Css.Foreign exposing (body, each, global)
import Css.Helpers exposing (toCssIdentifier)
import Html
import Html.Styled exposing (Html, button, div, fromUnstyled, li, styled, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (class, classList)
import Html.Styled.Events exposing (onClick)
import SelectList as SL


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


view : Model -> Html.Html Msg
view model =
    styled div
        []
        []
        [ bodyStyleNode
        , div [ class (toCssIdentifier DateBarClass) ]
            [ button [ onClick Previous, class (toCssIdentifier ButtonClass) ] [ text "Previous" ]
            , text <| .id <| SL.selected model.calendar
            , button [ onClick Next, class (toCssIdentifier ButtonClass) ] [ text "Next" ]
            ]
        , SL.selected model.calendar |> .tiles |> renderTiles
        , div [] [ text <| toString model ]
        ]
        |> toUnstyled


type alias Model =
    { calendar : SL.SelectList { id : String, tiles : List (List String) }
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


tiles3 : List (List String)
tiles3 =
    [ [ "ELM", "AI" ]
    , [ "e-commerce" ]
    ]


calendar : SL.SelectList { id : String, tiles : List (List String) }
calendar =
    SL.fromLists
        [ { id = "2018-05-14", tiles = tiles }
        , { id = "2018-05-15", tiles = tiles2 }
        ]
        { id = "2018-05-16", tiles = tiles3 }
        []


model : Model
model =
    Model calendar


type Msg
    = Previous
    | Next


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Previous ->
            ( { model | calendar = SL.select (\e -> e.id < (.id <| SL.selected model.calendar)) model.calendar }, Cmd.none )

        Next ->
            ( { model | calendar = SL.select (\e -> e.id > (.id <| SL.selected model.calendar)) model.calendar }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }
