module RecordList exposing (..)
import Browser
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border

import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (hidden)

import Tooltip exposing (..)
--import Array
import Html exposing (col)
import Array


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

--
-- MODEL
--

type alias Model =
    { fields: List Field 
    , records: List FieldData
    , matchStr: String
    , newFieldName: String
    }

type alias FieldData =
    { fieldName: String
    , values: List String
    }

type alias Field =
    { key : String
    , value : String
    }

initialModel : { fields : List { key : String, value : String }, records : List { fieldName : String, values : List String }, matchStr : String, newFieldName : String }
initialModel =
    { fields = 
        [ { key = "Field 1" , value = "Value 01" }
        , { key = "Field 2" , value = "Value 02" }
        , { key = "Field 3" , value = "Value 03" }
        , { key = "Field 4" , value = "Value 04" }
        ]
    , records = [ { fieldName = "Field 1"
                  , values = [ "Value 11", "Value 12" ] }
                , { fieldName = "Field 2"
                  , values = [ "Value 21", "Value 22" ] }
                , { fieldName = "Field 3"
                  , values = [ "Value 31", "Value 32" ] }
                , { fieldName = "Field 4"
                  , values = [ "Value 41", "Value 42" ] }
                ]
    , matchStr = ""
    , newFieldName = ""
    }

--
-- UPDATE
--

type Msg
    = AddFieldOfName String
    | FieldOfNameValue String String
    | AddRecord  
    | ToggleSelectRecord Int Bool
    | RemoveCheckedRecords
    | Match String
    | NewFieldName String

update : Msg -> Model -> Model
update msg model =
    case msg of 
        AddFieldOfName fieldName ->
            { model | fields = { key = fieldName, value = "" } :: model.fields
                    , records = addFieldDataForName fieldName model.records }
        FieldOfNameValue fieldName fieldValue ->
            { model | fields = 
                        List.map (\f -> if f.key == fieldName then
                                            { f | value = fieldValue }
                                        else
                                            f)
                                model.fields
            }
        AddRecord ->
            { model | records = addFieldValues model.fields model.records }
        ToggleSelectRecord idx checked ->
            model
        RemoveCheckedRecords ->
            model
        Match matchStr ->
            model
        NewFieldName str ->
            { model | newFieldName = str }

addFieldDataForName : String -> List FieldData -> List FieldData
addFieldDataForName name fieldData =
    let
        newFieldData = case fieldData of
            [] -> { fieldName = name, values = [] } 
            first :: rest -> { fieldName = name, values = List.repeat (List.length (.values first)) "" }   
    in
      newFieldData :: fieldData

addFieldValues fields records =
    List.map2 (\field data -> { data | values = field.value :: data.values }) fields records
--
-- VIEW
--

headerStyle : List  (Attribute Msg)
headerStyle = [ padding 5
              , Font.bold 
              , Background.color (Element.rgb255 150 150 238)
              , width fill]

myTextInput : String -> String -> (String -> msg) -> Element msg
myTextInput labelStr ref onChangeMsg =
    Input.text 
        [ width (fillPortion 10) ] 
        { label = Input.labelBelow 
            [Font.size 15] 
            (text labelStr)
        , onChange = always onChangeMsg text
        , text = ref 
        , placeholder = Nothing
        }

buttonStyle : String -> List (Attr () msg)
buttonStyle tip =
    [ Background.color (Element.rgb255 150 150 238)
    , mouseDown
        [ Background.color (Element.rgb255 167 167 255) ]
    , focused [ ]
    , centerX
    , tooltip above (myTooltip tip)
    , padding 5
    , Border.rounded 5
    , width (fillPortion 1)
    ]

myButton : String -> String -> Maybe msg -> Element msg
myButton labelStr tipStr onPressMsg =
    Input.button 
        (buttonStyle tipStr)
        { onPress = onPressMsg
        , label = el [centerX] (text labelStr)
        }

-- change color based on even/uneven row and checked status
rowStyle idx checked =
    [ htmlAttribute (hidden True)
    , width fill
    , height fill
    , padding 5
    , Background.color (case (modBy 2 idx, checked) of
        (1, True) ->  rgb255 170 200 200
        (1, False) ->  rgb255 200 200 200
        (0, True) -> rgb255 210 240 240
        (0, False) -> rgb255 240 240 240
        _ -> rgb255 0 0 0)
    ]


view : Model -> Html Msg
view model =

    layout [] <|  
        row [] 
        [ column [] 
            [ row [padding 5, width fill] 
                [ myTextInput 
                    "New Field Name"
                    model.newFieldName
                    NewFieldName
                , myButton 
                    "+" 
                    "Add New Field to Records"
                    (Just (AddFieldOfName  
                            model.newFieldName))
                ]
                , row [padding 5, width fill] 
                [ myButton 
                    "+" 
                    "Add Data to Table"
                    (Just AddRecord)
                ]    
                , row [padding 5, width fill] 
                [ Element.table [ padding 10 ]
                    { data = model.fields 
                    , columns =
                    [ { header = el headerStyle  ( Element.text "Fields" )
                      , width = fill
                      , view =
                      \field -> 
                          myTextInput 
                              field.key
                              field.value
                              (FieldOfNameValue field.key)
                      }
                    ]
                    }
                ]
            ]
            , column [width fill]
            (List.append
                -- table header with field Names
                [row [width fill]
                    (List.map (\fieldData ->
                        el headerStyle ( Element.text fieldData.fieldName ))
                        model.records)
                ]
                -- row of value columns
                [ row [Element.explain Debug.todo,  width fill]
                (List.map (\fieldData ->
                    -- value column for one field data value list
                    column [height fill, width fill]
                    (List.indexedMap 
                        (\i value -> el (rowStyle i False) (Element.text value))
                        fieldData.values))
                    model.records)
                ]
            )
        ]

