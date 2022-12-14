module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (hidden)

import Tooltip exposing (..)

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


type alias Model =
    { firstName: String
    , lastName: String 
    , persons: List Person
    , matchStr: String
    , addresses: List Address
    }

type alias Person =
    { firstName : String
    , lastName : String
    , checked : Bool
    }

type alias Address =
    { street : String
    , number: String
    , city: String
    , zip: String
    }

type Msg
    = FirstName String
    | LastName String
    | AddPerson Person 
    | ToggleSelectPerson Int Bool
    | RemoveChecked
    | Match String

initialModel: Model
initialModel = 
    { firstName = ""
    , lastName =""
    , persons = [ 
      { firstName = "David"
      , lastName = "Bowie"
      , checked = False
      }
    , { firstName = "Florence"
      , lastName = "Welch"
      , checked = False
      }
    ]
    , matchStr = ""
    , addresses = []
    }

setCheckedOnPerson : Int -> Bool -> (Int, Person) -> Person
setCheckedOnPerson idx check (index, person) =
    if idx == index
    then
        { person | checked = check }
    else
        person

update : Msg -> Model -> Model
update msg model =
    case msg of
        FirstName firstName ->
            { model | firstName = firstName }
        LastName lastName ->
            { model | lastName = lastName }
        AddPerson person  ->
            { model | persons = person :: model.persons  }
        ToggleSelectPerson idx checked ->
            { model | persons 
                = List.map 
                    (setCheckedOnPerson idx checked ) 
                    (List.indexedMap Tuple.pair model.persons) }
        RemoveChecked ->
            { model | persons = List.filter (\p -> not p.checked) model.persons }
        Match str ->
            { model | matchStr = str }

headerStyle : List  (Attribute Msg)
headerStyle = [ padding 5, Font.bold , Background.color (Element.rgb255 150 150 238) ]

-- change color based on even/uneven row and checked status
rowStyle idx checked =
    [ htmlAttribute (hidden True)
    , padding 5
    , Background.color (case (modBy 2 idx, checked) of
        (1, True) ->  rgb255 120 150 150
        (1, False) ->  rgb255 150 150 150
        (0, True) -> rgb255 225 255 255
        (0, False) -> rgb255 255 255 255
        _ -> rgb255 0 0 0)
    ]

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

myButton labelStr tipStr onPressMsg =
    Input.button 
        (buttonStyle tipStr)
        { onPress = onPressMsg
        , label = el [centerX] (text labelStr)
        }

myTextInput labelStr ref onChangeMsg =
    Input.text 
        [ width (fillPortion 10) ] 
        { label = Input.labelBelow 
            [Font.size 15] 
            (text labelStr) -- l
        , onChange = always onChangeMsg text
        , text = ref 
        , placeholder = Nothing
        }

view : Model -> Html Msg
view model =
    layout [] <|  column [] [
      row [  padding 10, spacing 10 ]
        [ myTextInput 
            "First Name"
            model.firstName
            FirstName 
        , myTextInput 
            "Last Name"
            model.lastName
            LastName
        , myButton 
            "+" 
            "Add Name Data to Table"
            (Just (AddPerson 
                        { firstName = model.firstName
                        , lastName = model.lastName
                        , checked = False }))
        , myButton 
            "-"
            "Remove Checked Rows from Table"
            (Just RemoveChecked) 
        ]
        , row 
          [ padding 10, width fill ]
          [ myTextInput 
            "Search"
            model.matchStr
            Match 
          ]
        , row 
          [ width fill ] 
          [ Element.indexedTable [ padding 10 ]
            { data = List.filter (\p ->   
                                    String.contains 
                                        model.matchStr 
                                        p.firstName
                                    ||
                                    String.contains 
                                        model.matchStr
                                        p.lastName)
                                        model.persons
            , columns =
               [    { header = el headerStyle  ( Element.text "First Name" )
                  , width = fillPortion 2
                  , view =
                        \idx person -> el (rowStyle idx person.checked)
                            (Element.text person.firstName)
                  }
                , { header = el headerStyle  (Element.text "Last Name")
                  , width = fillPortion 2
                  , view =
                        \idx person -> el (rowStyle idx person.checked)
                            (Element.text person.lastName)
                  }
                , { header = el headerStyle (Element.text "select")
                   , width = fillPortion 1 
                   , view =
                        \idx person ->
                            Input.checkbox [centerX, centerY, width fill]
                                { onChange = ToggleSelectPerson idx 
                                , icon = Input.defaultCheckbox
                                , checked = person.checked 
                                , label = Input.labelHidden "checked"
                                }
                   }
                ]
            }
        ]
    ]

