module Main exposing (Coords, Grid, Msg(..))

import Browser
import Html exposing (..)
import Html.Attributes as Attributes
    exposing
        ( attribute
        , class
        , classList
        , disabled
        , height
        , href
        , property
        , style
        , width
        )
import Html.Events exposing (onClick, onInput)
import Json.Encode as JE
import Set exposing (Set)
import Set.Extra as Set
import Time


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Coords =
    ( Int, Int )


type alias Grid =
    { height : Int
    , width : Int
    , activeSet : Set Coords
    , blockSize : Int
    }


type alias Model =
    { currentGrid : Grid
    , oldGrids : List Grid
    , gameRunning : Bool
    , pause : Float
    }


type Msg
    = PopGrid
    | MsgGrid MsgGrid
    | LauchGame
    | StopGame
    | ChangePause Float
    | NoOp


type MsgGrid
    = NewWidth Int
    | NewHeight Int
    | NewBlockSize Int
    | Toogle Coords
    | VertSym
    | HorizSym
    | Reduce
    | DrawCross
    | DrawSmile
    | DrawConwayGun
    | Clear
    | ConwaysStep


toBlockSize : String -> Msg
toBlockSize s =
    case String.toInt s of
        Just size ->
            MsgGrid <| NewBlockSize size

        Nothing ->
            NoOp


toChangePause : String -> Msg
toChangePause s =
    case String.toInt s of
        Just t ->
            ChangePause <| toFloat t

        Nothing ->
            NoOp


toHeight : String -> Msg
toHeight s =
    case String.toInt s of
        Just h ->
            MsgGrid <| NewHeight h

        Nothing ->
            NoOp


toWidth : String -> Msg
toWidth s =
    case String.toInt s of
        Just w ->
            MsgGrid <| NewWidth w

        Nothing ->
            NoOp


init () =
    ( { currentGrid =
            { height = 100
            , width = 100
            , activeSet = Set.empty
            , blockSize = 2
            }
      , oldGrids = []
      , gameRunning = False
      , pause = 50
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameRunning then
        Sub.map MsgGrid <| Time.every model.pause (\_ -> ConwaysStep)

    else
        Sub.none


view : Model -> Html Msg
view model =
    let
        buttonGrid : MsgGrid -> String -> Html Msg
        buttonGrid msgGrid label =
            button [ onClick <| MsgGrid msgGrid, disabled model.gameRunning ]
                [ text label ]
    in
    div []
        [ div
            []
            [ h1 [] [ text "Parameterizable Game of Life" ]
            , p [ style "max-width" "40em" ]
                [ text "This is a "
                , a [ href "https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life" ] [ text "Game of Life" ]
                , text """ generator. You can either click
                on the grid to (de)activate a cell or use the buttons to
                build a schema (try the gun !). Once you're happy with your
                grid, click on "Launch" and watch the film of the life!"""
                ]
            , text "Height:"
            , input
                [ attribute "type" "number"
                , onInput toHeight
                , property "value" (JE.string <| String.fromInt model.currentGrid.height)
                , Attributes.min "0"
                , disabled model.gameRunning
                ]
                []
            , text "Width:"
            , input
                [ attribute "type" "number"
                , onInput toWidth
                , property "value" (JE.string <| String.fromInt model.currentGrid.width)
                , Attributes.min "0"
                , disabled model.gameRunning
                ]
                []
            , text "Block size:"
            , input
                [ attribute "type" "number"
                , onInput toBlockSize
                , property "value" (JE.string <| String.fromInt model.currentGrid.blockSize)
                , Attributes.min "0"
                , disabled model.gameRunning
                ]
                []
            , br [] []
            , buttonGrid VertSym "Vertical reverse"
            , buttonGrid HorizSym "Horizontal reverse"
            , buttonGrid Reduce "Reduce"
            , br [] []
            , text "Draw: "
            , buttonGrid DrawCross "Cross"
            , buttonGrid DrawSmile "Smile"
            , buttonGrid DrawConwayGun "Gosper glide gun"
            , br [] []
            , text "Game of Life: "
            , buttonGrid ConwaysStep "Step"
            , if model.gameRunning then
                button [ onClick StopGame ] [ text "Stop" ]

              else
                button [ onClick LauchGame ] [ text "Launch" ]
            , text "Pause between each step:"
            , input
                [ attribute "type" "number"
                , onInput toChangePause
                , property "value" (JE.string <| String.fromInt <| floor model.pause)
                , Attributes.min "25"
                ]
                []
            , br [] []
            , button [ onClick PopGrid, disabled model.gameRunning ] [ text "Cancel" ]
            , buttonGrid Clear "Clear"
            ]
        , br [] []
        , div [] [ Html.map MsgGrid <| viewGrid model.currentGrid ]
        ]


viewGrid : Grid -> Html MsgGrid
viewGrid grid =
    let
        cell y x =
            td
                [ onClick <| Toogle ( x, y )
                , width grid.blockSize
                , height grid.blockSize
                , classList [ ( "active", Set.member ( x, y ) grid.activeSet ) ]
                ]
                []

        row y =
            tr []
                (List.map
                    (cell y)
                    (List.range 0 (grid.width - 1))
                )
    in
    table [ class "grid" ] (List.map row <| List.range 0 (grid.height - 1))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                PopGrid ->
                    case model.oldGrids of
                        [] ->
                            model

                        grid :: otherGrids ->
                            { model | currentGrid = grid, oldGrids = otherGrids }

                ChangePause pause ->
                    { model | pause = pause }

                MsgGrid msgGrid ->
                    if model.gameRunning && msgGrid /= ConwaysStep then
                        model

                    else
                        { model
                            | currentGrid = updateGrid msgGrid model.currentGrid
                            , oldGrids = model.currentGrid :: model.oldGrids
                        }

                LauchGame ->
                    { model | gameRunning = True }

                StopGame ->
                    { model | gameRunning = False }

                NoOp ->
                    model
    in
    ( newModel, Cmd.none )


updateGrid : MsgGrid -> Grid -> Grid
updateGrid msg grid =
    case msg of
        NewBlockSize s ->
            { grid | blockSize = s }

        NewWidth w ->
            { grid | width = w }

        NewHeight h ->
            { grid | height = h }

        Toogle coords ->
            if Set.member coords grid.activeSet then
                { grid | activeSet = Set.remove coords grid.activeSet }

            else
                { grid | activeSet = Set.insert coords grid.activeSet }

        VertSym ->
            { grid | activeSet = Set.map (\( x, y ) -> ( x, grid.height - y - 1 )) grid.activeSet }

        HorizSym ->
            { grid | activeSet = Set.map (\( x, y ) -> ( grid.height - x - 1, y )) grid.activeSet }

        Reduce ->
            { grid
                | height = grid.height // 2
                , width = grid.width // 2
                , activeSet =
                    Set.filterMap
                        (\( x, y ) ->
                            case ( modBy 2 x, modBy 2 y ) of
                                ( 1, 1 ) ->
                                    Just ( x // 2, y // 2 )

                                _ ->
                                    Nothing
                        )
                        grid.activeSet
            }

        Clear ->
            { grid | activeSet = Set.empty }

        DrawCross ->
            let
                size =
                    min grid.height grid.width

                newPixels =
                    Set.fromList
                        (List.map2 Tuple.pair (List.range 1 size) (List.range 1 size)
                            ++ List.map2 Tuple.pair
                                (List.range 1 size)
                                (List.range 1 size |> List.reverse)
                        )
            in
            { grid | activeSet = Set.union newPixels grid.activeSet }

        DrawSmile ->
            let
                size =
                    min grid.height grid.width

                radius =
                    size // 2

                head =
                    circle radius radius radius

                eye_left =
                    circle (size // 3) (size // 3) (radius // 6)

                eye_right =
                    circle (2 * size // 3) (size // 3) (radius // 6)

                noze =
                    circle (size // 2) (size // 2) (radius // 8)

                mouth =
                    arc (size // 2) (size // 2) (radius // 2) (pi / 4) (3 * pi / 4)
            in
            { grid | activeSet = unionList [ grid.activeSet, head, eye_left, eye_right, noze, mouth ] }

        DrawConwayGun ->
            { grid | activeSet = Set.union grid.activeSet gosperGlideGunCoords }

        ConwaysStep ->
            let
                mW =
                    modBy grid.width

                mH =
                    modBy grid.height

                neighborhood ( x, y ) =
                    [ ( x, y )
                    , ( mW (x + 1), y )
                    , ( mW (x + 1), mH (y + 1) )
                    , ( mW (x + 1), mH (y - 1) )
                    , ( x, mH (y + 1) )
                    , ( x, mH (y - 1) )
                    , ( mW (x - 1), y )
                    , ( mW (x - 1), mH (y + 1) )
                    , ( mW (x - 1), mH (y - 1) )
                    ]

                candidates =
                    Set.foldr
                        (\( x, y ) accu ->
                            List.foldr
                                Set.insert
                                accu
                                (neighborhood ( x, y ))
                        )
                        alived
                        alived

                count : List comparable -> Set comparable -> Int
                count elts set =
                    List.foldr
                        (\elt accu ->
                            if Set.member elt set then
                                accu + 1

                            else
                                accu
                        )
                        0
                        elts

                alived =
                    grid.activeSet

                isAliveNext cell =
                    let
                        nbAlived =
                            count (neighborhood cell) alived
                    in
                    if Set.member cell alived then
                        nbAlived == 3 || nbAlived == 4

                    else
                        nbAlived == 3
            in
            { grid | activeSet = Set.filter isAliveNext candidates }


unionList : List (Set comparable) -> Set comparable
unionList sets =
    List.foldr Set.union Set.empty sets


arc : Int -> Int -> Int -> Float -> Float -> Set ( Int, Int )
arc cx cy radius start end =
    let
        n =
            8 * radius

        iStart =
            floor (toFloat n * start / 2 / pi)

        iEnd =
            floor (toFloat n * end / 2 / pi)
    in
    List.map
        (\i ->
            ( cx + floor (toFloat radius * cos (toFloat i * 2 * pi / toFloat n))
            , cy + floor (toFloat radius * sin (toFloat i * 2 * pi / toFloat n))
            )
        )
        (List.range iStart iEnd)
        |> Set.fromList


circle : Int -> Int -> Int -> Set ( Int, Int )
circle cx cy radius =
    arc cx cy radius 0 (2 * pi)


gosperGlideGunCoords =
    Set.fromList [ ( 5, 1 ), ( 5, 2 ), ( 6, 1 ), ( 6, 2 ), ( 5, 11 ), ( 6, 11 ), ( 7, 11 ), ( 4, 12 ), ( 3, 13 ), ( 3, 14 ), ( 8, 12 ), ( 9, 13 ), ( 9, 14 ), ( 6, 15 ), ( 4, 16 ), ( 5, 17 ), ( 6, 17 ), ( 7, 17 ), ( 6, 18 ), ( 8, 16 ), ( 3, 21 ), ( 4, 21 ), ( 5, 21 ), ( 3, 22 ), ( 4, 22 ), ( 5, 22 ), ( 2, 23 ), ( 6, 23 ), ( 1, 25 ), ( 2, 25 ), ( 6, 25 ), ( 7, 25 ), ( 3, 35 ), ( 4, 35 ), ( 3, 36 ), ( 4, 36 ) ]
