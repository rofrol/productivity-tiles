module Main exposing (main)

import Css exposing (..)
import Css.Foreign exposing (body, global)
import Css.Helpers exposing (toCssIdentifier)
import Html
import Html.Styled exposing (Html, button, div, fromUnstyled, li, styled, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (class, classList)


bodyStyleNode : Html msg
bodyStyleNode =
    global
        [ body
            [ backgroundColor (hex "#fff")
            , color (hex "#000")
            , fontFamilies [ "sans-serif" ]
            , boxSizing borderBox
            ]
        ]


type DivClasses
    = Div1Class
    | TilesClass
    | RowClass
    | TileClass


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
        ]


renderTiles : List (List String) -> Html msg
renderTiles tiles =
    tiles
        |> List.map
            (\row ->
                div [ class (toCssIdentifier RowClass) ]
                    (row
                        |> List.map
                            (\tile ->
                                div [ class (toCssIdentifier TileClass) ] [ text tile ]
                            )
                    )
            )
        |> (::) tilesStyleNode
        |> div [ class (toCssIdentifier TilesClass) ]


view : Model -> Html.Html msg
view model =
    styled div
        []
        []
        [ bodyStyleNode
        , div [] [ text <| toString model ]
        , renderTiles model.tiles
        ]
        |> toUnstyled


type alias Model =
    { tiles : List (List String)
    }


model : Model
model =
    Model
        [ [ "ELM", "AI" ]
        , [ "Rust", "e-commerce" ]
        ]


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }
