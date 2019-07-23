module HashDict exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Fuzz exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra as List
import HashingContainers.HashDict as HashDict exposing (HashDict)
import Typeclasses.Classes.Equality as Equality
import Typeclasses.Classes.Hashing as Hashing
import Random
import Random.List


listComparison : Test
listComparison =
  describe "Comparison to List"
    [
      fuzz (list (tuple (string, int))) "fromList-toList" <| \ list ->
        equal (list |> List.reverse |> List.uniqueBy Tuple.first |> List.sort)
          (list |> HashDict.fromList Equality.string (Hashing.string 3) |> HashDict.toList |> List.sort)
    ]

dictComparison : Test
dictComparison =
  describe "Comparison to Dict"
    [
      let
        fuzzer = list (tuple (string, int))
        in fuzz2 fuzzer fuzzer "remove" <| \ toKeep toDelete ->
          let
            list =
              toKeep |> (++) toDelete |> Random.List.shuffle |>
              Random.step |> (|>) (Random.initialSeed 0) |> Tuple.first
            expected =
              toDelete |>
              List.foldl (\ (key, _) -> Dict.remove key) (Dict.fromList list) |>
              Dict.toList |> List.sort
            hashDict =
              HashDict.fromList Equality.string (Hashing.string 3) list
            actual =
              toDelete |>
              List.foldl (\ (key, _) -> HashDict.remove key) hashDict |>
              HashDict.toList |> List.sort
            in equal expected actual
      ,
      fuzz2 (tuple (string, int)) (list (tuple (string, int))) "lookup" <| \ head list ->
        let
          shuffledList =
            head :: list |> Random.List.shuffle |>
            Random.step |> (|>) (Random.initialSeed 0) |> Tuple.first
          expected =
            Dict.fromList shuffledList |> Dict.get (Tuple.first head)
          hashDict =
            HashDict.fromList Equality.string (Hashing.string 3) shuffledList
          actual =
            HashDict.lookup (Tuple.first head) hashDict
          in equal expected actual
    ]
