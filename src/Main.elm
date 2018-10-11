module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, h2, header, img, li, main_, nav, p, section, span, text, ul)
import Html.Attributes exposing (class, disabled, href, src, style)
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
    , drinks : Dict DrinkId RemoteDrink
    , drinkIdRouteHistory : List DrinkId
    }


type DrinkSection
    = OverviewSection
    | InstructionsSection


type Route
    = RandomDrinkRoute (Maybe String)
    | DrinkRoute DrinkSection DrinkId
    | NotFoundRoute


type RemoteDrink
    = Loading
    | LoadingThumbs Drink Int
    | Success Drink
    | Failure String


type alias DrinkId =
    String


type alias Drink =
    { id : DrinkId
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
    changePage url
        { navKey = navKey
        , route = RandomDrinkRoute Nothing
        , drinks = Dict.empty
        , drinkIdRouteHistory = []
        }



-- Update


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | GotoPreviousDrink
    | GotoNextDrink
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

        GotoPreviousDrink ->
            let
                backCmd =
                    case model.route of
                        DrinkRoute currentSection currentDrinkId ->
                            let
                                maybePrevDrinkId =
                                    let
                                        history =
                                            model.drinkIdRouteHistory
                                                |> Array.fromList
                                    in
                                    case findIndexOf 0 currentDrinkId history of
                                        Just i ->
                                            Array.get (i + 1) history

                                        Nothing ->
                                            Nothing
                            in
                            case maybePrevDrinkId of
                                Just prevDrinkId ->
                                    DrinkRoute currentSection prevDrinkId
                                        |> toHref
                                        |> Nav.pushUrl model.navKey

                                Nothing ->
                                    Cmd.none

                        _ ->
                            Cmd.none
            in
            ( model, backCmd )

        GotoNextDrink ->
            let
                nextCmd =
                    case model.route of
                        DrinkRoute currentSection currentDrinkId ->
                            let
                                maybeNextDrinkId =
                                    let
                                        history =
                                            model.drinkIdRouteHistory
                                                |> Array.fromList
                                    in
                                    case findIndexOf 0 currentDrinkId history of
                                        Just i ->
                                            Array.get (i - 1) history

                                        Nothing ->
                                            Nothing
                            in
                            case maybeNextDrinkId of
                                Just nextDrinkId ->
                                    DrinkRoute currentSection nextDrinkId
                                        |> toHref
                                        |> Nav.pushUrl model.navKey

                                Nothing ->
                                    RandomDrinkRoute Nothing
                                        |> toHref
                                        |> Nav.pushUrl model.navKey

                        RandomDrinkRoute (Just failure) ->
                            RandomDrinkRoute Nothing
                                |> toHref
                                |> Nav.pushUrl model.navKey

                        _ ->
                            Cmd.none
            in
            ( model, nextCmd )

        GotRandomDrink result ->
            case result of
                Ok (Just drink) ->
                    case model.route of
                        RandomDrinkRoute _ ->
                            ( { model | drinks = Dict.insert drink.id (LoadingThumbs drink 0) model.drinks }
                            , DrinkRoute OverviewSection drink.id
                                |> toHref
                                |> Nav.replaceUrl model.navKey
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    let
                        nextRoute =
                            case model.route of
                                RandomDrinkRoute _ ->
                                    RandomDrinkRoute (Just "Couldn't get random drink :/")

                                _ ->
                                    model.route
                    in
                    ( { model | route = nextRoute }, Cmd.none )

        GotDrink id result ->
            case result of
                Ok (Just drink) ->
                    ( { model | drinks = Dict.insert id (LoadingThumbs drink 0) model.drinks }, Cmd.none )

                Ok Nothing ->
                    ( { model | drinks = Dict.insert id (Failure "Drink not found") model.drinks }, Cmd.none )

                Err _ ->
                    ( { model | drinks = Dict.insert id (Failure "Could not get drink") model.drinks }, Cmd.none )

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
        [ P.map (RandomDrinkRoute Nothing) P.top
        , P.map (DrinkRoute OverviewSection) (P.s "drink" </> P.string)
        , P.map (DrinkRoute InstructionsSection) (P.s "drink" </> P.string </> P.s "instructions")
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
        RandomDrinkRoute _ ->
            "/"

        DrinkRoute section id ->
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

        drinkIdRouteHistory =
            case nextRoute of
                DrinkRoute _ drinkId ->
                    let
                        history =
                            case model.route of
                                RandomDrinkRoute _ ->
                                    -- If we got a random drink that we already have
                                    -- shown, forget the last history
                                    List.filter (\id -> id /= drinkId) model.drinkIdRouteHistory

                                _ ->
                                    model.drinkIdRouteHistory
                    in
                    if List.member drinkId model.drinkIdRouteHistory then
                        history

                    else
                        drinkId :: history

                _ ->
                    model.drinkIdRouteHistory

        ( drinks, nextCmd ) =
            case nextRoute of
                RandomDrinkRoute _ ->
                    ( model.drinks, sendGetRandomDrink )

                DrinkRoute section id ->
                    case Dict.get id model.drinks of
                        Just (Failure _) ->
                            -- Try again
                            ( Dict.insert id Loading model.drinks, sendGetDrinkById id )

                        Nothing ->
                            ( model.drinks, sendGetDrinkById id )

                        _ ->
                            ( model.drinks, Cmd.none )

                _ ->
                    ( model.drinks, Cmd.none )
    in
    ( { model
        | route = nextRoute
        , drinks = drinks
        , drinkIdRouteHistory = drinkIdRouteHistory
      }
    , nextCmd
    )



-- Commands


sendGetDrinkById : DrinkId -> Cmd Msg
sendGetDrinkById id =
    getDrinkById id |> Http.send (GotDrink id)


sendGetRandomDrink : Cmd Msg
sendGetRandomDrink =
    Http.send GotRandomDrink getRandomDrink



-- Requests


getDrinkById : DrinkId -> Http.Request (Maybe Drink)
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


findIndexOf : Int -> a -> Array a -> Maybe Int
findIndexOf i val arr =
    case Array.get i arr of
        Just currVal ->
            if currVal == val then
                Just i

            else
                findIndexOf (i + 1) val arr

        Nothing ->
            Nothing


canGotoPrevDrink : Route -> List DrinkId -> Bool
canGotoPrevDrink currentRoute drinkIdRouteHistory =
    List.length drinkIdRouteHistory
        > 1
        && (case currentRoute of
                DrinkRoute _ currentDrinkId ->
                    Just currentDrinkId /= (List.reverse drinkIdRouteHistory |> List.head)

                _ ->
                    -- We uses current route drink id to be our current history position
                    -- and here we have no drink id to go on
                    False
           )


currentDrinkName default route drinks =
    case route of
        DrinkRoute _ id ->
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


viewApp : Model -> Html Msg
viewApp model =
    main_ [ class "font-sans h-full flex flex-col text-grey-darkest" ]
        [ viewTopBar (currentDrinkName "Preparing drink.." model.route model.drinks) (canGotoPrevDrink model.route model.drinkIdRouteHistory)
        , viewRoute model
        , viewTabNav model.route
        ]


viewRoute : Model -> Html Msg
viewRoute { route, drinks } =
    let
        viewContent =
            case route of
                NotFoundRoute ->
                    div [] [ text "Oops, drink not found" ]

                RandomDrinkRoute maybeFailure ->
                    case maybeFailure of
                        Just failure ->
                            viewRandomDrinkFailed failure

                        Nothing ->
                            viewRandomDrink

                DrinkRoute section id ->
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


viewRandomDrink =
    viewRemoteDrink Loading (\_ -> text "")


viewRandomDrinkFailed failure =
    div [] [ text failure ]


viewRemoteDrink remoteDrink viewWhenSuccess =
    let
        spinner =
            case remoteDrink of
                Loading ->
                    viewSpinner

                LoadingThumbs _ _ ->
                    viewSpinner

                _ ->
                    text ""

        content =
            case remoteDrink of
                Loading ->
                    text ""

                LoadingThumbs drink _ ->
                    viewLoadingThumbs drink

                Success drink ->
                    viewWhenSuccess drink

                Failure _ ->
                    div [] [ text "Couldn't get the specific drink :/" ]
    in
    div [ class "h-full relative" ]
        [ spinner -- Always render spinner so we dont restart animation on state transitions
        , content
        ]


viewLoadingThumbs : Drink -> Html Msg
viewLoadingThumbs drink =
    let
        thumbUrls =
            drink.thumbUrl :: List.map .thumbUrl drink.ingredients

        images =
            List.map (\url -> img [ on "load" (D.succeed <| GotDrinkThumbImage drink.id), src url ] []) thumbUrls
    in
    div [ style "display" "none" ] images


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
                            [ img [ class "block h-24 mr-1", src ingredient.thumbUrl ] []
                            , text <| ingredient.measure ++ " " ++ ingredient.name
                            ]
                    )
            )
        , div [ class "p-2 pb-8 whitespace-pre-wrap leading-normal" ] [ text drink.instructions ]
        ]


viewTopNav prevEnabled =
    div [ class "flex justify-between m-1 text-base" ]
        [ button
            [ class <|
                "flex items-center "
                    ++ (if not prevEnabled then
                            "cursor-default text-grey"

                        else
                            "text-blue-dark"
                       )
            , disabled (not prevEnabled)
            , onClick GotoPreviousDrink
            ]
            [ viewChevronLeft, text " Prev" ]
        , button [ class "flex items-center text-blue-dark", onClick GotoNextDrink ] [ text "Next ", viewChevronRight ]
        ]


viewTopBar caption prevEnabled =
    header [ class "flex-no-shrink pb-1 text-3xl text-center truncate" ]
        [ viewTopNav prevEnabled
        , text caption
        ]


viewTabNav : Route -> Html msg
viewTabNav currentRoute =
    let
        maybeCurrentDrinkId =
            case currentRoute of
                DrinkRoute _ id ->
                    Just id

                _ ->
                    Nothing

        viewLink svgIcon extraClasses caption url =
            li [ class "" ]
                [ a [ class (extraClasses ++ " text-xs flex flex-col items-center w-24 no-underline pb-1 pt-2"), href url ]
                    [ svgIcon
                    , text caption
                    ]
                ]

        viewNavLink svgIcon section =
            let
                active =
                    case currentRoute of
                        DrinkRoute currentSection _ ->
                            section == currentSection

                        _ ->
                            False

                disabled =
                    maybeCurrentDrinkId == Nothing

                url =
                    case maybeCurrentDrinkId of
                        Just id ->
                            toHref (DrinkRoute section id)

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


viewSpinner =
    div [ class "flex h-full items-center justify-center" ] [ viewSvgSpinner ]



-- Svgs


viewChevronLeft =
    svg [ SvgAttr.viewBox "0 0 185.3 185.3", SvgAttr.class "h-5" ] [ Svg.path [ SvgAttr.fill "currentColor", SvgAttr.stroke "none", SvgAttr.transform "scale(-1, 1) translate(-185.3, 0)", SvgAttr.d "M51.7 185.3a10.7 10.7 0 0 1-7.6-18.3l74.4-74.3L44 18.3A10.7 10.7 0 1 1 59.3 3.1l82 82c4.1 4.2 4.1 11 0 15.2l-82 81.9c-2.1 2-4.8 3.1-7.6 3.1z" ] [] ]


viewChevronRight =
    svg [ SvgAttr.viewBox "0 0 185.3 185.3", SvgAttr.class "h-5" ] [ Svg.path [ SvgAttr.fill "currentColor", SvgAttr.stroke "none", SvgAttr.d "M51.7 185.3a10.7 10.7 0 0 1-7.6-18.3l74.4-74.3L44 18.3A10.7 10.7 0 1 1 59.3 3.1l82 82c4.1 4.2 4.1 11 0 15.2l-82 81.9c-2.1 2-4.8 3.1-7.6 3.1z" ] [] ]


viewSvgOverview =
    svg [ SvgAttr.viewBox "0 0 323.4 323.4", SvgAttr.class "h-6 mb-1" ] [ Svg.path [ SvgAttr.fill "currentColor", SvgAttr.stroke "none", SvgAttr.d "M297.7 53.3c0-35-68.4-53.3-136-53.3s-136 18.3-136 53.3c0 12.1 8.3 22.3 22 30.3l104 101.3v118.5h-47.5a10 10 0 1 0 0 20h115a10 10 0 1 0 0-20h-47.5V185L276 83.5c13.5-8 21.7-18.1 21.7-30.2zM76 32C98.6 24.3 129 20 161.7 20s63 4.3 85.9 12.1c20.8 7.2 30.1 15.8 30.1 21.2 0 5.3-9.3 14-30.1 21.1-22.8 7.8-53.3 12.1-85.9 12.1s-63-4.3-85.8-12C55 67.2 45.7 58.5 45.7 53.2c0-5.4 9.3-14 30.2-21.2zm85.8 134.7l-69-67c21 4.5 45.1 6.8 69 6.8 24 0 48-2.3 69-6.9l-69 67z" ] [] ]


viewSvgInstructions =
    svg [ SvgAttr.viewBox "0 0 411.4 411.4", SvgAttr.class "h-6 mb-1" ] [ Svg.path [ SvgAttr.fill "currentColor", SvgAttr.stroke "none", SvgAttr.d "M248.7 22.1H245V10a10 10 0 0 0-20 0v12.1h-40V10a10 10 0 0 0-20 0v12.1h-40V10a10 10 0 0 0-20 0v12.1H65V10a10 10 0 0 0-20 0v12.1h-3.7c-22 0-40 18-40 40v309.3c0 22 18 40 40 40h207.4c22 0 40-18 40-40V62.1c0-22-18-40-40-40zm20 349.3c0 11-9 20-20 20H41.3c-11 0-20-9-20-20V62.1c0-11 9-20 20-20H45v25.7a10 10 0 0 0 20 0V42.1h40v25.7a10 10 0 0 0 20 0V42.1h40v25.7a10 10 0 0 0 20 0V42.1h40v25.7a10 10 0 0 0 20 0V42.1h3.7c11 0 20 9 20 20v309.3z" ] [], Svg.path [ SvgAttr.fill "currentColor", SvgAttr.stroke "none", SvgAttr.d "M234.5 156.5h-179a10 10 0 0 0 0 20h179a10 10 0 0 0 0-20zM234.5 216.5h-179a10 10 0 0 0 0 20h179a10 10 0 0 0 0-20zM234.5 276.5h-179a10 10 0 1 0 0 20h179a10 10 0 0 0 0-20zM406 42.5a10 10 0 0 0-5.9-8.1 81 81 0 0 0-33.5-8 81 81 0 0 0-33.6 8 10 10 0 0 0-5.8 8.1c-5.5 55-5.5 108.6 0 163.7a10 10 0 0 0 5.6 8l6.8 139.4c0 .7.1 1.3.3 2l11.5 44.2a10 10 0 0 0 9.7 7.5h11c4.5 0 8.5-3.1 9.7-7.5l11.5-44.2.3-2 6.8-139.4a10 10 0 0 0 5.6-8c5.5-55 5.5-108.6 0-163.7zm-59.5 8c3.5-1.4 6.9-2.4 10-3v74.2a10 10 0 0 0 20 0V47.4a69 69 0 0 1 10.2 3c4.5 49.5 4.5 97.7 0 147h-40.3a797.6 797.6 0 0 1 .1-147zm27.1 301l-7 27.2-7-27.1-6.6-134.2h27.2l-6.6 134.2z" ] [] ]


viewSvgRepo =
    svg [ SvgAttr.viewBox "0 0 24 24", SvgAttr.class "h-6 mb-1" ] [ Svg.path [ SvgAttr.fill "currentColor", SvgAttr.stroke "none", SvgAttr.d "M12 .3a12 12 0 0 0-3.8 23.4c.6.1.8-.3.8-.6v-2c-3.3.7-4-1.6-4-1.6-.6-1.4-1.4-1.8-1.4-1.8-1-.7.1-.7.1-.7 1.2 0 1.9 1.2 1.9 1.2 1 1.8 2.8 1.3 3.5 1 0-.8.4-1.3.7-1.6-2.7-.3-5.5-1.3-5.5-6 0-1.2.5-2.3 1.3-3.1-.2-.4-.6-1.6 0-3.2 0 0 1-.3 3.4 1.2a11.5 11.5 0 0 1 6 0c2.3-1.5 3.3-1.2 3.3-1.2.6 1.6.2 2.8 0 3.2.9.8 1.3 1.9 1.3 3.2 0 4.6-2.8 5.6-5.5 5.9.5.4.9 1 .9 2.2v3.3c0 .3.1.7.8.6A12 12 0 0 0 12 .3" ] [] ]


viewSvgSpinner =
    svg
        [ SvgAttr.viewBox "0 0 44 44"
        , SvgAttr.class "h-32"
        , SvgAttr.stroke "currentColor"
        ]
        [ Svg.g
            [ SvgAttr.fill "none"
            , SvgAttr.fillRule "evenodd"
            , SvgAttr.strokeWidth "2"
            ]
            [ Svg.circle
                [ SvgAttr.cx "22"
                , SvgAttr.cy "22"
                , SvgAttr.r "1"
                ]
                [ Svg.animate
                    [ SvgAttr.attributeName "r"
                    , SvgAttr.begin "0s"
                    , SvgAttr.dur "1.8s"
                    , SvgAttr.values "1; 20"
                    , SvgAttr.calcMode "spline"
                    , SvgAttr.keyTimes "0; 1"
                    , SvgAttr.keySplines "0.165, 0.84, 0.44, 1"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                , Svg.animate
                    [ SvgAttr.attributeName "stroke-opacity"
                    , SvgAttr.begin "0s"
                    , SvgAttr.dur "1.8s"
                    , SvgAttr.values "1; 0"
                    , SvgAttr.calcMode "spline"
                    , SvgAttr.keyTimes "0; 1"
                    , SvgAttr.keySplines "0.3, 0.61, 0.355, 1"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                ]
            , Svg.circle
                [ SvgAttr.cx "22"
                , SvgAttr.cy "22"
                , SvgAttr.r "1"
                ]
                [ Svg.animate
                    [ SvgAttr.attributeName "r"
                    , SvgAttr.begin "-0.9s"
                    , SvgAttr.dur "1.8s"
                    , SvgAttr.values "1; 20"
                    , SvgAttr.calcMode "spline"
                    , SvgAttr.keyTimes "0; 1"
                    , SvgAttr.keySplines "0.165, 0.84, 0.44, 1"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                , Svg.animate
                    [ SvgAttr.attributeName "stroke-opacity"
                    , SvgAttr.begin "-0.9s"
                    , SvgAttr.dur "1.8s"
                    , SvgAttr.values "1; 0"
                    , SvgAttr.calcMode "spline"
                    , SvgAttr.keyTimes "0; 1"
                    , SvgAttr.keySplines "0.3, 0.61, 0.355, 1"
                    , SvgAttr.repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        ]
