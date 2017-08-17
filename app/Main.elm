module Main exposing (..)

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MODEL


type alias Wine =
    { id : Int, wineType : String, name : String }


type alias Food =
    { id : Int, wineIds : List Int, name : String }


type alias Model =
    { wines : List Wine, foods : List Food, selected : String }


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
    [ Food 1 [ 10, 12 ] "Lasagna"
    , Food 2 [ 5, 12 ] "Grilled Vegetables"
    , Food 3 [ 4, 2, 8, 6 ] "Sushi"
    , Food 4 [ 5, 1, 15 ] "Grilled Fish"
    , Food 5 [ 13, 11, 14 ] "Steak Tacos"
    , Food 6 [ 11, 14 ] "Filet Minon"
    , Food 7 [ 9, 14 ] "Ribs"
    , Food 8 [ 1, 3, 4, 8, 7 ] "Mixed Green Salad"
    , Food 9 [ 2, 16 ] "Fois Gras"
    , Food 10 [ 15, 16 ] "Strong Cheese"
    , Food 11 [ 2, 8 ] "Blue Cheese"
    , Food 12 [ 2, 3, 9 ] "Thai Food"
    , Food 13 [ 16, 8 ] "Dessert"
    , Food 14 [ 10, 9, 15 ] "Burger"
    , Food 15 [ 8 ] "Hot Dog"
    , Food 16 [ 13 ] "Chili"
    , Food 17 [ 1, 3, 4, 5, 6, 14 ] "Crackers"
    , Food 18 [ 7, 12 ] "Mac and Cheese"
    , Food 19 [ 1 ] "Snails"
    , Food 20 [ 2 ] "Other"
    ]


initialModel : Model
initialModel =
    { wines = initialWines
    , foods = initialFoods
    , selected = ""
    }



-- UPDATE


type Msg
    = ChooseWine Wine
    | ChooseFood Food
    | SearchWines String
    | SearchFoods String
    | ResetModel


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChooseWine wine ->
            { model
                | foods = List.filter (\f -> findPairings f.wineIds wine.id) initialFoods
                , selected = wine.name
            }

        ChooseFood food ->
            { model
                | wines = List.filter (\w -> findPairings food.wineIds w.id) initialWines
                , selected = food.name
            }

        SearchWines searchText ->
            { model
                | wines = List.filter (\w -> String.contains (String.toLower searchText) (String.toLower w.name)) initialWines
            }

        SearchFoods searchText ->
            { model
                | foods = List.filter (\f -> String.contains (String.toLower searchText) (String.toLower f.name)) initialFoods
            }

        ResetModel ->
            initialModel


findPairings : List Int -> Int -> Bool
findPairings wineIds id =
    let
        matches =
            wineIds
                |> List.filter (\someId -> someId == id)
                |> List.length
    in
    matches > 0



--VIEW


displaySelected : String -> Html Msg
displaySelected selectedName =
    if selectedName /= "" then
        div [ class "row featurette" ]
            [ div [ class "col-md-7" ]
                [ h2 [] [ text ("You have selected " ++ selectedName) ]
                ]
            ]
    else
        div [] []


displayWines : List Wine -> Html Msg
displayWines wines =
    div []
        [ div [ class "row featurette" ]
            [ div [ class "col-md-4" ]
                [ h2 [] [ text "Filter by Wine" ]
                , p [ class "lead" ] [ text "Click on a wine type or search and then select desired wine" ]
                , input [ placeholder "Wine Type", onInput SearchWines ] []
                ]
            , displayWineCategory "White" (List.filter (\n -> n.wineType == "White") wines)
            , displayWineCategory "Red" (List.filter (\n -> n.wineType == "Red") wines)
            ]
        ]


displayWineCategory : String -> List Wine -> Html Msg
displayWineCategory title wines =
    if List.length wines > 0 then
        div [ class "col-md-8" ]
            [ h3 [] [ text title ]
            , div [ class "row" ] (List.map displayWine wines)
            ]
    else
        div [] []


displayWine : Wine -> Html Msg
displayWine wine =
    div [ class "col-md-3 hoverable", onClick (ChooseWine wine) ] [ text wine.name ]


displayFoods : List Food -> Html Msg
displayFoods foods =
    div []
        [ div [ class "row featurette" ]
            [ div [ class "col-md-4" ]
                [ h2 [] [ text "Filter by Food" ]
                , p [ class "lead" ] [ text "Click on a food item or search and then select desired food" ]
                , input [ placeholder "Food Item", onInput SearchFoods ] []
                ]
            , div [ class "col-md-8" ]
                [ div [ class "row" ] (List.map displayFood foods)
                ]
            ]
        ]


displayFood : Food -> Html Msg
displayFood food =
    div [ class "col-md-3 hoverable", onClick (ChooseFood food) ] [ text food.name ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "nav-bar" ]
            [ h1 [] [ text "Pairing Lab" ]
            ]
        , div [ class "container container-sup" ]
            [ div [ class "top-border" ] []
            ,p [ class "lead" ] [ text
            """
            An intelligent way to pair food and wine.
            This interactive and comprehensive approach allows anyone and everyone to pair the perfect wine.
            To get going, just select the food you are planning to eat to get wines that pair. Alternatively, filter out matching foods by the bottle you have.
            """ ]
            , displaySelected model.selected
            , hr [ class "hot" ] []
            , displayWines model.wines
            , hr [] []
            , displayFoods model.foods
            , hr [ class "hot" ] []
            , button [ class "btn btn-primary btn-lg btn-block", onClick ResetModel ] [ text "Reset" ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
