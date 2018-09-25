module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (Html, a, button, div, h1, h2, img, li, p, text, ul)
import Html.Attributes exposing (class, href, src, style)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as D
import Url exposing (Url)


-- Main


main =
    Browser.application
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- Model


type alias Model =
    { navKey : Nav.Key
    , page : Page
    }


type Page
    = PageDrink RemoteDrink
    | PageNotFound


type RemoteDrink
    = Loading DrinkId
    | LoadingThumbs Drink Int
    | Success Drink
    | Failure String


type DrinkId
    = DrinkId String
    | RandomDrinkId


type alias Drink =
    { id : String
    , name : String
    , thumbUrl : String
    , instructions : String
    , ingredients : List Ingredient
    , isAlcoholic : Bool
    }


type alias Ingredient =
    { name : String
    , measure : String
    , thumbUrl : String
    }



-- Init


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changePage url { navKey = navKey, page = PageNotFound }



-- Update


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | GotDrink (Result Http.Error (Maybe Drink))
    | GotDrinkThumbImage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            changePage url model

        GotDrink (Ok maybeDrink) ->
            case maybeDrink of
                Just drink ->
                    ( { model | page = PageDrink (LoadingThumbs drink 0) }, Nav.replaceUrl model.navKey drink.id )

                Nothing ->
                    ( { model | page = PageNotFound }, Cmd.none )

        GotDrink (Err _) ->
            ( { model | page = PageDrink (Failure "Ops") }, Cmd.none )

        GotDrinkThumbImage forDrinkId ->
            case model.page of
                PageDrink (LoadingThumbs drink nbOfLoadedThumbs) ->
                    if drink.id == forDrinkId then
                        let
                            allThumbsLoaded =
                                nbOfLoadedThumbs + 1 == 1 + List.length drink.ingredients

                            remoteDrink =
                                if allThumbsLoaded then
                                    Success drink
                                else
                                    LoadingThumbs drink (nbOfLoadedThumbs + 1)
                        in
                        ( { model | page = PageDrink remoteDrink }, Cmd.none )
                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


changePage : Url -> Model -> ( Model, Cmd Msg )
changePage url model =
    let
        parsedDrinkId =
            drinkIdFromUrl url
    in
    if parsedDrinkId == currentDrinkId model.page then
        ( model, Cmd.none )
    else
        case parsedDrinkId of
            Just id ->
                ( { model | page = PageDrink (Loading id) }, sendGetDrinkById id )

            Nothing ->
                ( { model | page = PageNotFound }, Cmd.none )


currentDrinkId : Page -> Maybe DrinkId
currentDrinkId page =
    case page of
        PageDrink (Loading id) ->
            Just id

        PageDrink (LoadingThumbs { id } _) ->
            Just (DrinkId id)

        PageDrink (Success { id }) ->
            Just (DrinkId id)

        _ ->
            Nothing


drinkIdFromUrl : Url -> Maybe DrinkId
drinkIdFromUrl url =
    let
        id =
            String.dropLeft 1 url.path
    in
    case id of
        "" ->
            Just RandomDrinkId

        possibleId ->
            case String.toInt possibleId of
                Just _ ->
                    Just (DrinkId possibleId)

                Nothing ->
                    Nothing



-- Commands


sendGetDrinkById id =
    Http.send GotDrink (getDrinkById id)



-- Requests


getDrinkById : DrinkId -> Http.Request (Maybe Drink)
getDrinkById drinkId =
    let
        url =
            case drinkId of
                RandomDrinkId ->
                    "https://www.thecocktaildb.com/api/json/v1/1/random.php"

                DrinkId id ->
                    "https://www.thecocktaildb.com/api/json/v1/1/lookup.php?i=" ++ id
    in
    Http.get url decodeDrinks



-- Decoding


decodeDrinks : D.Decoder (Maybe Drink)
decodeDrinks =
    D.field "drinks" (D.nullable D.value)
        |> D.andThen
            (\maybeDrinks ->
                if maybeDrinks == Nothing then
                    D.succeed Nothing
                else
                    D.maybe decodeDrink
            )


decodeDrink : D.Decoder Drink
decodeDrink =
    (D.field "drinks" <| D.index 0 <| D.dict (D.nullable D.string))
        |> D.andThen
            (\dict ->
                let
                    getField name defaultValue =
                        Dict.get name dict |> Maybe.withDefault Nothing |> Maybe.withDefault defaultValue

                    ingredients =
                        List.range 1 15
                            |> List.map
                                (\i ->
                                    let
                                        name =
                                            getField ("strIngredient" ++ String.fromInt i) "" |> String.trim
                                    in
                                    { name = name
                                    , measure = getField ("strMeasure" ++ String.fromInt i) "" |> String.trim
                                    , thumbUrl = "https://www.thecocktaildb.com/images/ingredients/" ++ name ++ "-Small.png"
                                    }
                                )
                            |> List.filter (\ingredient -> ingredient.name /= "")

                    isAlcoholic =
                        "Alcoholic" == getField "strAlcoholic" ""

                    id =
                        getField "idDrink" ""
                in
                if id == "" then
                    D.fail "Missing id"
                else
                    D.succeed
                        { id = id
                        , name = getField "strDrink" "Untitled"
                        , thumbUrl = getField "strDrinkThumb" "thumb-missing.png"
                        , instructions = getField "strInstructions" "No instructions available"
                        , ingredients = ingredients
                        , isAlcoholic = isAlcoholic
                        }
            )



-- Helpers


getPageTitle : Page -> String
getPageTitle page =
    case page of
        PageDrink (LoadingThumbs drink _) ->
            drink.name

        PageDrink (Success drink) ->
            drink.name

        _ ->
            "Random Drink"



-- View


view : Model -> Browser.Document Msg
view model =
    Browser.Document (getPageTitle model.page)
        [ div [ class "container" ] [ viewPage model.page ]
        ]


viewPage page =
    case page of
        PageNotFound ->
            div [] [ text "Oops, drink not found" ]

        PageDrink remoteDrink ->
            case remoteDrink of
                Loading id ->
                    viewLoading id

                LoadingThumbs drink _ ->
                    viewLoadingThumbs drink

                Success drink ->
                    viewDrink drink

                Failure _ ->
                    div [] [ text "Couldn't get the specific drink :/" ]


viewLoading id =
    let
        what =
            case id of
                RandomDrinkId ->
                    "random drink"

                DrinkId actualId ->
                    "drink #" ++ actualId
    in
    div [] [ text <| "Please wait for " ++ what ++ ".." ]


viewLoadingThumbs : Drink -> Html Msg
viewLoadingThumbs drink =
    let
        thumbUrls =
            drink.thumbUrl :: List.map .thumbUrl drink.ingredients

        images =
            List.map (\url -> img [ on "load" (D.succeed <| GotDrinkThumbImage drink.id), src url ] []) thumbUrls
    in
    div []
        [ text ("Preparing drink " ++ drink.name ++ "..")
        , div [ style "display" "none" ]
            images
        ]


viewDrink : Drink -> Html Msg
viewDrink drink =
    div []
        [ div [ class "page" ]
            [ h1 [ class "drink-headline" ] [ text drink.name ]
            , img [ src drink.thumbUrl, class "drink-thumb" ] []
            , p [ class "instructions" ] [ text drink.instructions ]
            , a [ class "link", href "/" ] [ text "Another drink plz" ]
            ]
        , div [ class "ingredients" ]
            [ div []
                [ h2 [ class "ingredients-headline" ] [ text "Ingredients" ]
                , ul [ class "ingredients-list" ]
                    (drink.ingredients
                        |> List.map
                            (\ingredient ->
                                li [ class "ingredient-item" ]
                                    [ img [ class "ingredient-thumb", src ingredient.thumbUrl ] []
                                    , text <| ingredient.measure ++ " " ++ ingredient.name
                                    ]
                            )
                    )
                ]
            , div []
                [ div [ class "footer" ]
                    [ a [ class "repo-link", href "https://github.com/axelo/random-drink" ] [ text "github" ]
                    ]
                ]
            ]
        ]
