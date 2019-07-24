module HashingContainers.HashSet exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.HashTrie as HashTrie exposing (HashTrie)
import HashingContainers.Extensions.Maybe as Maybe


type alias HashSet value = HashTrie value value

empty : Equality value -> Hashing value -> HashSet value
empty equality hashing = HashTrie.empty equality hashing identity

fromList : Equality value -> Hashing value -> List value -> HashSet value
fromList equality hashing = HashTrie.fromList equality hashing identity

fromArray : Equality value -> Hashing value -> Array value -> HashSet value
fromArray equality hashing = HashTrie.fromArray equality hashing identity

insert : value -> HashSet value -> HashSet value
insert = HashTrie.insert

remove : value -> HashSet value -> HashSet value
remove = HashTrie.remove

member : value -> HashSet value -> Bool
member value = HashTrie.lookup value >> Maybe.match False (always True)

isEmpty : HashSet value -> Bool
isEmpty = HashTrie.isEmpty

foldl : (value -> result -> result) -> result -> HashSet value -> result
foldl = HashTrie.foldl

toList : HashSet value -> List value
toList = HashTrie.toList
