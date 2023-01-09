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
import Html exposing (col)
import Char exposing (isHexDigit)
import Html.Attributes exposing (value)


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
    { records: List FieldData
    , matchStr: String
    , newFieldName: String
    }

type alias FieldData =
    { fieldName: String
    , newValue: String
    , position: Int
    , values: List String
    }


initialModel : Model
initialModel =
    { records = [ { fieldName = "Field 1"
                  , newValue = "Value 01"
                  , position = 1
                  , values = [ "Rest 01", "Quest 01", "Test 01", "Value 11", "Value 12" ] }
                , { fieldName = "Field 2"
                  , newValue = "Value 02"
                  , position = 2
                  , values = [ "Rest 02", "Quest 02", "Test 02", "Value 21", "Value 22" ] }
                , { fieldName = "Field 3"
                  , newValue = "Value 03"
                  , position = 3
                  , values = [ "Rest 03", "Quest 03", "Test 03", "Value 31", "Value 32" ] }
                , { fieldName = "Field 4"
                  , newValue = "Value 04"
                  , position = 4
                  , values = [ "Rest 03", "Quest 03", "Test 03", "Value 41", "Value 42" ] }
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
    | FieldOfNamePosition String String
    | AddRecord  
    | ClearData
    | ToggleSelectRecord Int Bool
    | RemoveCheckedRecords
    | Match String
    | NewFieldName String

update : Msg -> Model -> Model
update msg model =
    case msg of 
        AddFieldOfName fieldName ->
            { model | records = 
                    List.map (\record -> { record | position = record.position + 1 })
                        (addFieldDataForName fieldName model.records) }
        FieldOfNameValue fieldName fieldValue ->
            { model | records = 
                        List.map (\r -> if r.fieldName == fieldName then
                                            { r | newValue = fieldValue }
                                        else
                                            r)
                                model.records
            }
        
        {- change position of field of given fieldName 
        to given fieldPostion (must be Int in  [1 .. records.length])
        and  move other fields accordingly to stay in the range of [1 .. records.length] 
        -}
        FieldOfNamePosition fieldName fieldPosition ->
            let
                newPos = Maybe.withDefault 0 (String.toInt fieldPosition)
                currentPos = List.sum
                    (List.map 
                        (\r -> 
                            if r.fieldName == fieldName then 
                                r.position 
                            else 
                                0)
                        model.records)
            in
            if List.member newPos (List.range 1 (List.length model.records)) 
                && newPos /= currentPos then
            let
                op = if currentPos < newPos then (-) else (+)
                rnge = List.range (min currentPos newPos) (max currentPos newPos)
            in
            { model | records = List.sortBy .position 
                (List.map (\r -> if r.fieldName == fieldName then
                                     { r | position = newPos }
                                else
                                    { r | position = 
                                        if List.member r.position rnge  then
                                            op r.position 1
                                        else
                                            r.position } 
                                    )
                                model.records)
            }
            else
                model
        AddRecord ->
            { model | records = addFieldValues model.records }
        ClearData ->
            { model | records = 
                List.map (\r -> { r | newValue = ""}) model.records }
        ToggleSelectRecord idx checked ->
            model
        RemoveCheckedRecords ->
            model
        Match str ->
            { model | matchStr = str }
        NewFieldName str ->
            { model | newFieldName = str }

addFieldDataForName : String -> List FieldData -> List FieldData
addFieldDataForName name fieldData =
    let
        newFieldData = case fieldData of
            [] -> 
                { fieldName = name
                , newValue = ""
                , position = 0
                , values = [] } 
            
            first :: rest -> 
                { fieldName = name
                , newValue = ""
                , position = 0
                , values = List.repeat (List.length (.values first)) "" }   
    in
      newFieldData :: fieldData

addFieldValues records =
    (List.map (\record -> { record | values = record.newValue :: record.values }) records)

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
        [ width (fillPortion 4) ] 
        { label = Input.labelBelow 
            [Font.size 15] 
            (text labelStr)
        , onChange = always onChangeMsg text
        , text = ref 
        , placeholder = Nothing
        }

myIntInput labelStr ref onChangeMsg =
    Input.text 
        [ width (fillPortion 1) ] 
        { label = Input.labelBelow 
            [Font.size 15] 
            (text labelStr)
        , onChange = always onChangeMsg text
        , text = String.fromInt ref 
        , placeholder = Nothing
        }

buttonStyle tip tipPos =
    [ Background.color (Element.rgb255 150 150 238)
    , mouseDown
        [ Background.color (Element.rgb255 167 167 255) ]
    , focused [ ]
    , centerX
    , tooltip tipPos (myTooltip tip)
    , padding 5
    , Border.rounded 5
    , width (fillPortion 1)
    ]

myButton labelStr tipStr tipPos onPressMsg =
    Input.button 
        (buttonStyle tipStr tipPos)
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


combineRows records =
    case List.head records of
        Nothing -> []
        Just r ->
            List.foldr (\v1 v2 ->
                            List.map2 (++) v1 v2)
                (List.repeat (List.length (.values r) ) "")
                (List.map .values records)

view : Model -> Html Msg
view model =
    let 
        matchIndices = List.indexedMap 
                            (\idx row -> 
                                if (String.contains model.matchStr row) then
                                    idx
                                else
                                    -1
                            )
                            (combineRows model.records)

    in
    layout [] <|  
        row [] 
        [ column [alignTop] 
            [ row [padding 5, width fill] 
                [ myButton 
                    "CLR" 
                    "Clear Data"
                    onRight
                    (Just ClearData)
                ]
            , row [padding 5, width fill] 
                [ myButton 
                    "+" 
                    "Add Data to Table"
                    onRight
                    (Just AddRecord)
                ]
            , row [padding 3, width fill] 
                [ myTextInput 
                    "New Field Name"
                    model.newFieldName
                    NewFieldName
                , myButton 
                    "+" 
                    "Add New Field to Records"
                    onRight
                    (Just (AddFieldOfName  
                            model.newFieldName))
                ]
    
            ]
        , column [padding 3, width fill]
            (List.concat
                -- Data entry for fields
                [[ row [width fill]
                    (List.map (\record ->
                        row [width fill]
                            [ myIntInput 
                                    "pos"
                                    record.position
                                    (FieldOfNamePosition record.fieldName) 
                                 , myTextInput 
                                    record.fieldName
                                    record.newValue
                                    (FieldOfNameValue record.fieldName) 
                            ] 
                        )
                        model.records)
                ]
                , [row 
                     [ padding 10, width fill ]
                    [ myTextInput 
                        "Search"
                        model.matchStr
                        Match 
                    ]]
                -- table header with field Names
                ,[row [width fill]
                    (List.map (\fieldData ->
                        el headerStyle ( Element.text fieldData.fieldName ))
                        model.records)
                ]
                -- row of value columns
                ,[ row [width fill]
                (List.map (\fieldData ->
                    -- value column for one field data value list
                    column [height fill, width fill]
                    (List.indexedMap 
                        (\i value -> 
                            el (rowStyle i False) (Element.text value))
                        (List.concat (List.indexedMap 
                            (\i value -> 
                                if List.member i matchIndices then 
                                    [value] 
                                else [] )                     
                                fieldData.values))))
                        model.records)
                ]]
            )
        ]

