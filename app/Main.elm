module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL


type alias Wine =
    { id : Int, wineType : String, name : String }


type alias Food =
    { id : Int, name : String }


type alias Model =
    { wines : List Wine, foods : List Food }


initialWines : List Wine
initialWines =
    [ Wine 1 "White" "Chardonnay"
    , Wine 2 "White" "Reisling"
    , Wine 3 "White" "Chenin Blanc"
    , Wine 4 "White" "Pinot Gris"
    , Wine 5 "White" "Pinot Blanc"
    , Wine 6 "White" "Gruner Vetliner"
    , Wine 7 "White" "Semillon"
    , Wine 8 "White" "Bubbles"
    , Wine 9 "Red" "Cabernet"
    , Wine 10 "Red" "Cab Franc"
    , Wine 11 "Red" "Syrah"
    , Wine 12 "Red" "Merlot"
    , Wine 13 "Red" "Tempernillo"
    , Wine 14 "Red" "Malbec"
    , Wine 15 "Red" "Pinot Noir"
    , Wine 16 "Red" "Port"
    ]


initialFoods : List Food
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
    div [ class "col-md-1", onClick (ChooseWine wine) ] [ text wine.name ]


displayWines : List Wine -> Html Msg
displayWines wines =
    div []
        [ h2 [ class "text-center" ] [ text "Wines" ]
        , h3 [] [ text "Whites" ]
        , div [ class "row" ] (List.map displayWine (List.filter (\n -> n.wineType == "White") wines))
        , h3 [] [ text "Reds" ]
        , div [ class "row" ] (List.map displayWine (List.filter (\n -> n.wineType == "Red") wines))
        ]


displayFood : Food -> Html Msg
displayFood food =
    div [ class "col-md-1", onClick (ChooseFood food) ] [ text food.name ]


displayFoods : List Food -> Html Msg
displayFoods foods =
    div []
        [ h2 [ class "text-center" ] [ text "Foods" ]
        , div [ class "row" ] (List.map displayFood foods)
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "nav-bar" ]
            [ h1 [] [ text "Pairing Lab" ]
            ]
        , div [ class "container container-sup" ]
            [ div [ class "top-border" ] []
            , p [ class "lead" ] [ text "An intelligent way to pair food and wine. This interactive and comprehensive approach allows anyone and everyone to pair the perfect wine." ]
            , hr [] []
            , displayWines model.wines
            , displayFoods model.foods
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
