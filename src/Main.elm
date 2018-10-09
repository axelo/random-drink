module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, h2, header, img, li, main_, nav, p, section, text, ul)
import Html.Attributes exposing (class, href, src, style)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as D
import Svg exposing (svg)
import Svg.Attributes as SvgAttr
import Url exposing (Url)
import Url.Parser as P exposing ((</>))



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
    , route : Route
    , drinks : Dict String RemoteDrink
    }


type DrinkSection
    = OverviewSection
    | InstructionsSection


type Route
    = HomeRoute
    | DrinkOverview DrinkSection String
    | NotFoundRoute


type RemoteDrink
    = Loading
    | LoadingThumbs Drink Int
    | Success Drink
    | Failure String


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
    changePage url { navKey = navKey, route = HomeRoute, drinks = Dict.empty }



-- Update


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | GotDrink String (Result Http.Error (Maybe Drink))
    | GotRandomDrink (Result Http.Error (Maybe Drink))
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

        GotRandomDrink result ->
            case result of
                Ok (Just drink) ->
                    let
                        nextModel =
                            { model | drinks = Dict.insert drink.id (LoadingThumbs drink 0) model.drinks }

                        nextCmd =
                            case model.route of
                                DrinkOverview _ "random" ->
                                    Nav.replaceUrl model.navKey (toHref (DrinkOverview OverviewSection drink.id))

                                _ ->
                                    Cmd.none
                    in
                    ( nextModel, nextCmd )

                _ ->
                    ( { model | drinks = Dict.insert "random" (Failure "dayum") model.drinks }, Cmd.none )

        GotDrink id result ->
            case result of
                Ok (Just drink) ->
                    ( { model | drinks = Dict.insert id (LoadingThumbs drink 0) model.drinks }, Cmd.none )

                Ok Nothing ->
                    ( { model | drinks = Dict.insert id (Failure "Not found yo") model.drinks }, Cmd.none )

                Err _ ->
                    ( { model | drinks = Dict.insert id (Failure "dayum") model.drinks }, Cmd.none )

        GotDrinkThumbImage id ->
            let
                ( nextModel, nextCmd ) =
                    case Dict.get id model.drinks of
                        Just (LoadingThumbs drink nbOfLoadedThumbs) ->
                            let
                                allThumbsLoaded =
                                    nbOfLoadedThumbs >= List.length drink.ingredients
                            in
                            if allThumbsLoaded then
                                ( { model | drinks = Dict.insert id (Success drink) model.drinks }, Cmd.none )

                            else
                                ( { model | drinks = Dict.insert id (LoadingThumbs drink (nbOfLoadedThumbs + 1)) model.drinks }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
            in
            ( nextModel, nextCmd )


routeParser : P.Parser (Route -> a) a
routeParser =
    P.oneOf
        [ P.map HomeRoute P.top
        , P.map (DrinkOverview OverviewSection) (P.s "drink" </> P.string)
        , P.map (DrinkOverview InstructionsSection) (P.s "drink" </> P.string </> P.s "instructions")
        ]


parseUrl : Url -> Route
parseUrl url =
    case P.parse routeParser url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


toHref : Route -> String
toHref route =
    case route of
        HomeRoute ->
            "/"

        DrinkOverview section id ->
            case section of
                OverviewSection ->
                    "/drink/" ++ id

                InstructionsSection ->
                    "/drink/" ++ id ++ "/instructions"

        NotFoundRoute ->
            ""


changePage : Url -> Model -> ( Model, Cmd Msg )
changePage url model =
    let
        nextRoute =
            parseUrl url

        nextCmd =
            case nextRoute of
                DrinkOverview _ "random" ->
                    sendGetRandomDrink

                DrinkOverview section id ->
                    case Dict.get id model.drinks of
                        Just (Failure _) ->
                            sendGetDrinkById id

                        Nothing ->
                            sendGetDrinkById id

                        _ ->
                            Cmd.none

                _ ->
                    Cmd.none
    in
    ( { model | route = nextRoute }, nextCmd )



-- Commands


sendGetDrinkById : String -> Cmd Msg
sendGetDrinkById id =
    getDrinkById id |> Http.send (GotDrink id)


sendGetRandomDrink : Cmd Msg
sendGetRandomDrink =
    Http.send GotRandomDrink getRandomDrink



-- Requests


getDrinkById : String -> Http.Request (Maybe Drink)
getDrinkById id =
    let
        url =
            "https://www.thecocktaildb.com/api/json/v1/1/lookup.php?i=" ++ id
    in
    Http.get url decodeDrinks


getRandomDrink : Http.Request (Maybe Drink)
getRandomDrink =
    let
        url =
            "https://www.thecocktaildb.com/api/json/v1/1/random.php"
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


currentDrinkName default route drinks =
    case route of
        DrinkOverview _ id ->
            case Dict.get id drinks of
                Just (LoadingThumbs drink _) ->
                    drink.name

                Just (Success drink) ->
                    drink.name

                _ ->
                    default

        _ ->
            default


sectionName section =
    case section of
        OverviewSection ->
            "Overview"

        InstructionsSection ->
            "Instructions"



-- View


view : Model -> Browser.Document Msg
view model =
    Browser.Document (currentDrinkName "Random Drink" model.route model.drinks)
        [ viewApp model ]


viewRoute : Model -> Html Msg
viewRoute { route, drinks } =
    let
        viewContent =
            case route of
                HomeRoute ->
                    div [] [ a [ href (toHref <| DrinkOverview OverviewSection "random") ] [ text "Get random drink" ] ]

                NotFoundRoute ->
                    div [] [ text "Oops, drink not found" ]

                DrinkOverview section id ->
                    let
                        remoteDrink =
                            Maybe.withDefault Loading (Dict.get id drinks)

                        viewSection =
                            case section of
                                OverviewSection ->
                                    viewDrink

                                InstructionsSection ->
                                    viewDrinkInstructions
                    in
                    viewRemoteDrink remoteDrink viewSection
    in
    section [ class "h-full overflow-auto scrolling-touch" ]
        [ viewContent
        ]


viewRemoteDrink remoteDrink viewWhenSuccess =
    case remoteDrink of
        Loading ->
            viewLoading

        LoadingThumbs drink _ ->
            viewLoadingThumbs drink

        Success drink ->
            viewWhenSuccess drink

        Failure _ ->
            div [] [ text "Couldn't get the specific drink :/" ]


viewLoading =
    div [] [ text <| "Please wait.." ]


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
    div [ class "h-full relative" ] [ img [ class "block max-w-full max-h-full absolute pin-center", src drink.thumbUrl ] [] ]


viewDrinkInstructions : Drink -> Html Msg
viewDrinkInstructions drink =
    div []
        [ ul [ class "list-reset" ]
            (drink.ingredients
                |> List.map
                    (\ingredient ->
                        li [ class "flex items-center py-2" ]
                            [ img [ class "block mr-1", src ingredient.thumbUrl ] []
                            , text <| ingredient.measure ++ " " ++ ingredient.name
                            ]
                    )
            )
        , div [ class "m-2 whitespace-pre-wrap leading-normal" ] [ text drink.instructions ]
        ]


viewApp : Model -> Html Msg
viewApp model =
    main_ [ class "font-sans h-full flex flex-col text-grey-darkest" ]
        [ viewTopBar (currentDrinkName "Random Drink" model.route model.drinks)
        , viewRoute model
        , viewTabNav model.route
        ]


viewTopBar caption =
    header [ class "flex-no-shrink py-4 text-2xl text-center border-b border-grey-light" ] [ text caption ]


viewTabNav : Route -> Html msg
viewTabNav currentRoute =
    let
        maybeCurrentDrinkId =
            case currentRoute of
                DrinkOverview _ id ->
                    Just id

                _ ->
                    Nothing

        viewLink svgIcon extraClasses caption url =
            li [ class "" ]
                [ a [ class (extraClasses ++ " flex flex-col items-center w-24 no-underline p-2"), href url ]
                    [ svgIcon
                    , text caption
                    ]
                ]

        viewNavLink svgIcon section =
            let
                active =
                    case currentRoute of
                        DrinkOverview currentSection _ ->
                            section == currentSection

                        _ ->
                            False

                disabled =
                    maybeCurrentDrinkId == Nothing

                url =
                    case maybeCurrentDrinkId of
                        Just id ->
                            toHref (DrinkOverview section id)

                        Nothing ->
                            ""

                extraClasses =
                    if disabled then
                        "cursor-default text-grey"

                    else if active then
                        "text-blue-dark"

                    else
                        "text-grey-darker"
            in
            viewLink svgIcon extraClasses (sectionName section) url
    in
    nav [ class "border-t border-grey-light flex-no-shrink" ]
        [ ul [ class "list-reset flex justify-around" ]
            [ viewNavLink viewSvgOverview OverviewSection
            , viewNavLink viewSvgInstructions InstructionsSection
            , viewLink viewSvgRepo "text-grey-darkest" "Source" "https://github.com/axelo/random-drink"
            ]
        ]



-- Svgs


viewSvgOverview =
    svg [ SvgAttr.viewBox "0 0 323.4 323.4", SvgAttr.class "h-8 mb-1" ] [ Svg.path [ SvgAttr.fill "currentColor", SvgAttr.stroke "none", SvgAttr.d "M297.7 53.3c0-35-68.4-53.3-136-53.3s-136 18.3-136 53.3c0 12.1 8.3 22.3 22 30.3l104 101.3v118.5h-47.5a10 10 0 1 0 0 20h115a10 10 0 1 0 0-20h-47.5V185L276 83.5c13.5-8 21.7-18.1 21.7-30.2zM76 32C98.6 24.3 129 20 161.7 20s63 4.3 85.9 12.1c20.8 7.2 30.1 15.8 30.1 21.2 0 5.3-9.3 14-30.1 21.1-22.8 7.8-53.3 12.1-85.9 12.1s-63-4.3-85.8-12C55 67.2 45.7 58.5 45.7 53.2c0-5.4 9.3-14 30.2-21.2zm85.8 134.7l-69-67c21 4.5 45.1 6.8 69 6.8 24 0 48-2.3 69-6.9l-69 67z" ] [] ]


viewSvgInstructions =
    svg [ SvgAttr.viewBox "0 0 411.4 411.4", SvgAttr.class "h-8 mb-1" ] [ Svg.path [ SvgAttr.fill "currentColor", SvgAttr.stroke "none", SvgAttr.d "M248.7 22.1H245V10a10 10 0 0 0-20 0v12.1h-40V10a10 10 0 0 0-20 0v12.1h-40V10a10 10 0 0 0-20 0v12.1H65V10a10 10 0 0 0-20 0v12.1h-3.7c-22 0-40 18-40 40v309.3c0 22 18 40 40 40h207.4c22 0 40-18 40-40V62.1c0-22-18-40-40-40zm20 349.3c0 11-9 20-20 20H41.3c-11 0-20-9-20-20V62.1c0-11 9-20 20-20H45v25.7a10 10 0 0 0 20 0V42.1h40v25.7a10 10 0 0 0 20 0V42.1h40v25.7a10 10 0 0 0 20 0V42.1h40v25.7a10 10 0 0 0 20 0V42.1h3.7c11 0 20 9 20 20v309.3z" ] [], Svg.path [ SvgAttr.fill "currentColor", SvgAttr.stroke "none", SvgAttr.d "M234.5 156.5h-179a10 10 0 0 0 0 20h179a10 10 0 0 0 0-20zM234.5 216.5h-179a10 10 0 0 0 0 20h179a10 10 0 0 0 0-20zM234.5 276.5h-179a10 10 0 1 0 0 20h179a10 10 0 0 0 0-20zM406 42.5a10 10 0 0 0-5.9-8.1 81 81 0 0 0-33.5-8 81 81 0 0 0-33.6 8 10 10 0 0 0-5.8 8.1c-5.5 55-5.5 108.6 0 163.7a10 10 0 0 0 5.6 8l6.8 139.4c0 .7.1 1.3.3 2l11.5 44.2a10 10 0 0 0 9.7 7.5h11c4.5 0 8.5-3.1 9.7-7.5l11.5-44.2.3-2 6.8-139.4a10 10 0 0 0 5.6-8c5.5-55 5.5-108.6 0-163.7zm-59.5 8c3.5-1.4 6.9-2.4 10-3v74.2a10 10 0 0 0 20 0V47.4a69 69 0 0 1 10.2 3c4.5 49.5 4.5 97.7 0 147h-40.3a797.6 797.6 0 0 1 .1-147zm27.1 301l-7 27.2-7-27.1-6.6-134.2h27.2l-6.6 134.2z" ] [] ]


viewSvgRepo =
    svg [ SvgAttr.viewBox "0 0 24 24", SvgAttr.class "h-8 mb-1" ] [ Svg.path [ SvgAttr.fill "currentColor", SvgAttr.stroke "none", SvgAttr.d "M12 .3a12 12 0 0 0-3.8 23.4c.6.1.8-.3.8-.6v-2c-3.3.7-4-1.6-4-1.6-.6-1.4-1.4-1.8-1.4-1.8-1-.7.1-.7.1-.7 1.2 0 1.9 1.2 1.9 1.2 1 1.8 2.8 1.3 3.5 1 0-.8.4-1.3.7-1.6-2.7-.3-5.5-1.3-5.5-6 0-1.2.5-2.3 1.3-3.1-.2-.4-.6-1.6 0-3.2 0 0 1-.3 3.4 1.2a11.5 11.5 0 0 1 6 0c2.3-1.5 3.3-1.2 3.3-1.2.6 1.6.2 2.8 0 3.2.9.8 1.3 1.9 1.3 3.2 0 4.6-2.8 5.6-5.5 5.9.5.4.9 1 .9 2.2v3.3c0 .3.1.7.8.6A12 12 0 0 0 12 .3" ] [] ]
