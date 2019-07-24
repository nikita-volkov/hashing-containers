module HashingContainers.HashDict exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.HashTrie as HashTrie exposing (HashTrie)


type alias HashDict key value =
  {
    equality : Equality key,
    hashing : Hashing key,
    trie : HashTrie (key, value)
  }

empty : Equality key -> Hashing key -> HashDict key value
empty equality hashing = HashDict equality hashing HashTrie.empty

fromFoldable :
  (((key, value) -> HashTrie (key, value) -> HashTrie (key, value)) -> HashTrie (key, value) -> foldable -> HashTrie (key, value)) ->
  Equality key -> Hashing key ->
  foldable -> HashDict key value
fromFoldable fold equality hashing foldable =
  HashDict equality hashing
    (HashTrie.fromFoldable
      fold
      (Hashing.map Tuple.first hashing).hash
      (Equality.map Tuple.first equality).eq
      foldable)

fromList : Equality key -> Hashing key -> List (key, value) -> HashDict key value
fromList = fromFoldable List.foldl

fromArray : Equality key -> Hashing key -> Array (key, value) -> HashDict key value
fromArray = fromFoldable Array.foldl

accessHashTrieAtKey : (Int -> ((key, value) -> Bool) -> HashTrie (key, value) -> result) -> key -> HashDict key value -> result
accessHashTrieAtKey fn key hashDict =
  let
    hash = hashDict.hashing.hash key
    eq = hashDict.equality.eq
    in fn hash (Tuple.first >> eq key) hashDict.trie

mapHashTrieAtKey : (Int -> ((key, value) -> Bool) -> HashTrie (key, value) -> HashTrie (key, value)) -> key -> HashDict key value -> HashDict key value
mapHashTrieAtKey fn key hashDict =
  { hashDict | trie = accessHashTrieAtKey fn key hashDict }

insert : key -> value -> HashDict key value -> HashDict key value
insert key value = mapHashTrieAtKey (HashTrie.insert (key, value)) key

remove : key -> HashDict key value -> HashDict key value
remove = mapHashTrieAtKey HashTrie.remove

update : key -> (Maybe value -> Maybe value) -> HashDict key value -> HashDict key value
update key updateFn = mapHashTrieAtKey (HashTrie.update (Maybe.map Tuple.second >> updateFn >> Maybe.map (Tuple.pair key))) key

lookup : key -> HashDict key value -> Maybe value
lookup key = accessHashTrieAtKey HashTrie.lookup key >> Maybe.map Tuple.second

isEmpty : HashDict key value -> Bool
isEmpty = .trie >> HashTrie.isEmpty

foldl : ((key, value) -> folding -> folding) -> folding -> HashDict key value -> folding
foldl step folding = .trie >> HashTrie.foldl step folding

toList : HashDict key value -> List (key, value)
toList = .trie >> HashTrie.toList
