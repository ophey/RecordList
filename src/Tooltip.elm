module Tooltip exposing (..)
-- copied from 
--   https://ellie-app.com/7R2VHTzHJYYa1 
-- as referenced here 
--   https://github.com/mdgriffith/elm-ui/issues/328#issuecomment-984427942

import Element exposing (..)
import Element.Font as Font
import Element.Border as Border
import Element.Background as Background
import Html.Attributes

myTooltip : String -> Element msg
myTooltip str =
    el
        [ Background.color (rgb 0 0 0)
        , Font.color (rgb 1 1 1)
        , padding 4
        , Border.rounded 5
        , Font.size 14
        , Border.shadow
            { offset = ( 0, 3 ), blur = 6, size = 0, color = rgba 0 0 0 0.32 }
        ]
        (text str)

tooltip : (Element msg -> Attribute msg) -> Element Never -> Attribute msg
tooltip usher tooltip_ =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , (usher << Element.map never) <|
                el [ htmlAttribute (Html.Attributes.style "pointerEvents" "none") ]
                    tooltip_
            ]
            none