module HashingContainers.HashSet exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.HashTrie as HashTrie exposing (HashTrie)
import HashingContainers.Extensions.Maybe as Maybe


type alias HashSet value =
  {
    equality : Equality value,
    hashing : Hashing value,
    trie : HashTrie value
  }

empty : Equality value -> Hashing value -> HashSet value
empty equality hashing = HashSet equality hashing HashTrie.empty

fromFoldable :
  ((value -> HashTrie value -> HashTrie value) -> HashTrie value -> foldable -> HashTrie value) ->
  Equality value -> Hashing value ->
  foldable -> HashSet value
fromFoldable fold equality hashing foldable =
  HashSet equality hashing
    (HashTrie.fromFoldable fold hashing.hash equality.eq foldable)

fromList : Equality value -> Hashing value -> List value -> HashSet value
fromList = fromFoldable List.foldl

fromArray : Equality value -> Hashing value -> Array value -> HashSet value
fromArray = fromFoldable Array.foldl

accessHashTrieAtKey : (Int -> (value -> Bool) -> HashTrie value -> result) -> value -> HashSet value -> result
accessHashTrieAtKey fn value hashSet =
  let
    hash = hashSet.hashing.hash value
    eq = hashSet.equality.eq
    in fn hash (eq value) hashSet.trie

mapHashTrieAtKey : (Int -> (value -> Bool) -> HashTrie value -> HashTrie value) -> value -> HashSet value -> HashSet value
mapHashTrieAtKey fn value hashSet =
  { hashSet | trie = accessHashTrieAtKey fn value hashSet }

insert : value -> HashSet value -> HashSet value
insert value = mapHashTrieAtKey (HashTrie.insert value) value

remove : value -> HashSet value -> HashSet value
remove = mapHashTrieAtKey HashTrie.remove

member : value -> HashSet value -> Bool
member value = accessHashTrieAtKey HashTrie.get value >> Maybe.match False (always True)

isEmpty : HashSet value -> Bool
isEmpty = .trie >> HashTrie.isEmpty

foldl : (value -> folding -> folding) -> folding -> HashSet value -> folding
foldl step folding = .trie >> HashTrie.foldl step folding

toList : HashSet value -> List value
toList = .trie >> HashTrie.toList
