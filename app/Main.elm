module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL


type alias Wine =
    { id : Int, name : String }


type alias Food =
    { id : Int, name : String }


type alias Model =
    { wines : List Wine, foods : List Food }


initialWines : List Wine
initialWines =
    [ Wine 1 "White"
    , Wine 2 "Red"
    ]


initialFoods : List Wine
initialFoods =
    [ Food 1 "Meat"
    , Food 2 "Veg"
    ]


initialModel : Model
initialModel =
    { wines = initialWines
    , foods = initialFoods
    }



-- UPDATE


type Msg
    = ChooseWine Wine
    | ChooseFood Food


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChooseWine wine ->
            { model | foods = List.filter (\f -> f.id == wine.id) initialFoods }

        ChooseFood food ->
            { model | wines = List.filter (\w -> w.id == food.id) initialWines }



--VIEW


displayWine : Wine -> Html Msg
displayWine wine =
    li [ onClick (ChooseWine wine) ] [ text wine.name ]


displayWines : List Wine -> Html Msg
displayWines wines =
    div []
        [ h2 [] [ text "Wines" ]
        , ul [] (List.map displayWine wines)
        ]


displayFood : Food -> Html Msg
displayFood food =
    li [ onClick (ChooseFood food) ] [ text food.name ]


displayFoods : List Food -> Html Msg
displayFoods foods =
    div []
        [ h2 [] [ text "Foods" ]
        , ul [] (List.map displayFood foods)
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Pairing Lab" ]
        , displayWines model.wines
        , displayFoods model.foods
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
