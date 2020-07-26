module Main exposing (main)

import AssocList
import Benchmark exposing (..)
import Benchmark.Runner as Runner
import Dict exposing (Dict)
import HashingContainers.HashDict as HashDict exposing (HashDict)
import IntDict exposing (IntDict)
import Typeclasses.Classes.Equality as Equality
import Typeclasses.Classes.Hashing as Hashing


main : Runner.BenchmarkProgram
main =
    Runner.program <|
        describe "Key value containers"
            [ describe "Core dict"
                [ Benchmark.benchmark "Insert 1 1" <|
                    \_ -> Dict.insert 1 1 itemsDict
                , Benchmark.benchmark "Insert 5000 1" <|
                    \_ -> Dict.insert 5000 1 itemsDict
                , Benchmark.benchmark "Get 1" <|
                    \_ -> Dict.get 1 itemsDict
                , Benchmark.benchmark "Get 5000" <|
                    \_ -> Dict.get 5000 itemsDict
                ]
            , describe "AssocList"
                [ Benchmark.benchmark "Insert 1 1" <|
                    \_ -> AssocList.insert 1 1 itemsAssocList
                , Benchmark.benchmark "Insert 5000 1" <|
                    \_ -> AssocList.insert 5000 1 itemsAssocList
                , Benchmark.benchmark "Get 1" <|
                    \_ -> AssocList.get 1 itemsAssocList
                , Benchmark.benchmark "Get 5000" <|
                    \_ -> AssocList.get 5000 itemsAssocList
                ]
            , describe "IntDict"
                [ Benchmark.benchmark "Insert 1 1" <|
                    \_ -> IntDict.insert 1 1 itemsIntDict
                , Benchmark.benchmark "Insert 5000 1" <|
                    \_ -> IntDict.insert 5000 1 itemsIntDict
                , Benchmark.benchmark "Get 1" <|
                    \_ -> IntDict.get 1 itemsIntDict
                , Benchmark.benchmark "Get 5000" <|
                    \_ -> IntDict.get 5000 itemsIntDict
                ]
            , describe "HashDict"
                [ Benchmark.benchmark "Insert 1 1" <|
                    \_ -> HashDict.insert 1 1 itemsHashDict
                , Benchmark.benchmark "Insert 5000 1" <|
                    \_ -> HashDict.insert 5000 1 itemsHashDict
                , Benchmark.benchmark "Get 1" <|
                    \_ -> HashDict.get 1 itemsHashDict
                , Benchmark.benchmark "Get 5000" <|
                    \_ -> HashDict.get 5000 itemsHashDict
                ]
            ]


range : List ( Int, Int )
range =
    List.map2 Tuple.pair (List.range 1 9999) (List.range 1 9999)


itemsDict : Dict Int Int
itemsDict =
    Dict.fromList range


itemsAssocList : AssocList.Dict Int Int
itemsAssocList =
    AssocList.fromList range


itemsHashDict : HashDict Int Int
itemsHashDict =
    HashDict.fromList Equality.int Hashing.int range


itemsIntDict : IntDict Int
itemsIntDict =
    IntDict.fromList range
