module Map exposing (..)

import Html exposing (Html)
import String
import Kintail.Cache as Cache


main : Html Never
main =
    let
        testStrings =
            [ "cat", "cat", "bat", "bat", "horse", "horse", "house", "house" ]

        lengthCache =
            Cache.memoize
                (\string ->
                    String.length (Debug.log "evaluating length of" string)
                )

        squareOfLengthCache =
            Cache.map
                (\n ->
                    let
                        _ =
                            Debug.log "evaluating square of" n
                    in
                        n * n
                )
                lengthCache

        squaredLengths strings cache =
            case strings of
                [] ->
                    []

                first :: rest ->
                    let
                        ( square, updatedCache ) =
                            Cache.query cache first
                    in
                        square :: squaredLengths rest updatedCache
    in
        Html.text (toString (squaredLengths testStrings squareOfLengthCache))
