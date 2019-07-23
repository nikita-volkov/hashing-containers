module HashingContainers.HashDict exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.HashTrie as HashTrie exposing (HashTrie)


type alias HashDict key value = HashTrie key (key, value)

empty : Equality key -> Hashing key -> HashDict key value
empty equality hashing = HashTrie.empty equality hashing Tuple.first

fromList : Equality key -> Hashing key -> List (key, value) -> HashDict key value
fromList equality hashing = HashTrie.fromList equality hashing Tuple.first

fromArray : Equality key -> Hashing key -> Array (key, value) -> HashDict key value
fromArray equality hashing = HashTrie.fromArray equality hashing Tuple.first

insert : key -> value -> HashDict key value -> HashDict key value
insert key value = HashTrie.insert (key, value)

remove : key -> HashDict key value -> HashDict key value
remove key = HashTrie.remove key

update : key -> (Maybe value -> Maybe value) -> HashDict key value -> HashDict key value
update key updateFn = HashTrie.update key (Maybe.map Tuple.second >> updateFn >> Maybe.map (Tuple.pair key))

lookup : key -> HashDict key value -> Maybe value
lookup key = HashTrie.lookup key >> Maybe.map Tuple.second

isEmpty : HashDict key value -> Bool
isEmpty = HashTrie.isEmpty

foldl : ((key, value) -> result -> result) -> result -> HashDict key value -> result
foldl = HashTrie.foldl

toList : HashDict key value -> List (key, value)
toList = HashTrie.toList
