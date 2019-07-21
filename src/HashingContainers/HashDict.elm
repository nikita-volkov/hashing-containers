module HashingContainers.HashDict exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.HashTrie as HashTrie exposing (HashTrie)


type alias HashDict key value = HashTrie (key, value)

empty : Equality key -> Hashing key -> Int -> HashDict key value
empty equality hashing expectedSizeAsPowerOfTwo =
  HashTrie.empty
    (Equality.map Tuple.first equality)
    (Hashing.map Tuple.first hashing)
    expectedSizeAsPowerOfTwo

fromList : Equality key -> Hashing key -> Int -> List (key, value) -> HashDict key value
fromList equality hashing expectedSizeAsPowerOfTwo =
  HashTrie.fromList
    (Equality.map Tuple.first equality)
    (Hashing.map Tuple.first hashing)
    expectedSizeAsPowerOfTwo
