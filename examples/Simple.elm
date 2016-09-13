module Simple exposing (..)

import Html exposing (Html)
import Kintail.Cache as Cache


main : Html Never
main =
    let
        testNumbers =
            [ 1, 1, 1, 2, 3, 3, 4, 4, 4, 5, 5 ]

        cache =
            Cache.memoize (\n -> Debug.log "evaluated" (n * n))

        squares numbers cache =
            case numbers of
                [] ->
                    []

                first :: rest ->
                    let
                        ( square, updatedCache ) =
                            Cache.query cache first
                    in
                        square :: squares rest updatedCache
    in
        Html.text (toString (squares testNumbers cache))
