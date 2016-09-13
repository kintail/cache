module Kintail.Cache
    exposing
        ( Cache
        , query
        , memoize
        , map
        )


type Cache i a
    = Cache (i -> ( a, Cache i a ))


query : Cache i a -> i -> ( a, Cache i a )
query (Cache implementation) =
    implementation


memoize : (i -> a) -> Cache i a
memoize function =
    Cache (evaluate function)


evaluate : (i -> a) -> i -> ( a, Cache i a )
evaluate function input =
    let
        output =
            function input
    in
        ( output, memoizedFunction function ( input, output ) )


memoizedFunction function ( lastInput, lastOutput ) =
    let
        implementation input =
            if input == lastInput then
                ( lastOutput, result )
            else
                evaluate function input

        result =
            Cache implementation
    in
        result


map : (a -> b) -> Cache i a -> Cache i b
map function cache =
    let
        implementation input =
            let
                ( intermediate, updatedCache ) =
                    query cache input

                output =
                    function intermediate

                newCache =
                    mappedCache function
                        updatedCache
                        ( intermediate, output )
            in
                ( output, newCache )
    in
        Cache implementation


mappedCache function cache (( lastIntermediate, lastOutput ) as tuple) =
    let
        implementation input =
            let
                ( intermediate, updatedCache ) =
                    query cache input
            in
                if intermediate == lastIntermediate then
                    ( lastOutput, mappedCache function updatedCache tuple )
                else
                    let
                        output =
                            function intermediate

                        newCache =
                            mappedCache function
                                updatedCache
                                ( intermediate, output )
                    in
                        ( output, newCache )
    in
        Cache implementation
