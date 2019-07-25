module HashingContainers.HashDict exposing
  (
    HashDict,
    empty,
    fromList,
    fromArray,
    insert,
    remove,
    update,
    get,
    isEmpty,
    foldl,
    toList
  )
{-|
HashDict API.

@docs HashDict

# Construction
@docs empty, fromList, fromArray

# Transformation
@docs insert, remove, update

# Access
@docs get, isEmpty, foldl, toList
-}

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.HashTrie as HashTrie exposing (HashTrie)


{-|
Generic dictionary from hashable keys to values.

Unlike `Dict` from the "core" library it is not
limited to any specific set of supported types for keys.

Unlike the association list implementations it does not suffer
from linear performance characteristics.
-}
type alias HashDict key value =
  {
    equality : Equality key,
    hashing : Hashing key,
    trie : HashTrie (key, value)
  }

{-|
Construct an empty HashDict,
providing instances of
[`Equality`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Equality#Equality)
and
[`Hashing`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Hashing#Hashing)
explicit typeclasses for key.
-}
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

{-|
Construct HashDict from a list of association pairs,
providing instances of
[`Equality`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Equality#Equality)
and
[`Hashing`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Hashing#Hashing)
explicit typeclasses for key.
-}
fromList : Equality key -> Hashing key -> List (key, value) -> HashDict key value
fromList = fromFoldable List.foldl

{-|
Construct HashDict from an array of association pairs,
providing instances of
[`Equality`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Equality#Equality)
and
[`Hashing`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Hashing#Hashing)
explicit typeclasses for key.
-}
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

{-|
Insert an association pair into dictionary,
replacing a value if it already exists.
-}
insert : key -> value -> HashDict key value -> HashDict key value
insert key value = mapHashTrieAtKey (HashTrie.insert (key, value)) key

{-|
Remove a key-value pair from a dictionary.
If the key is not found, no changes are made.
-}
remove : key -> HashDict key value -> HashDict key value
remove = mapHashTrieAtKey HashTrie.remove

{-|
Update the value of a dictionary for a specific key with a given function.
-}
update : key -> (Maybe value -> Maybe value) -> HashDict key value -> HashDict key value
update key updateFn = mapHashTrieAtKey (HashTrie.update (Maybe.map Tuple.second >> updateFn >> Maybe.map (Tuple.pair key))) key

{-|
Lookup a value by key.
Returns `Nothing`, when there is none.
-}
get : key -> HashDict key value -> Maybe value
get key = accessHashTrieAtKey HashTrie.get key >> Maybe.map Tuple.second

{-|
Determine if a dictionary is empty.
-}
isEmpty : HashDict key value -> Bool
isEmpty = .trie >> HashTrie.isEmpty

{-|
Fold over the key-value pairs in the dictionary.
-}
foldl : ((key, value) -> folding -> folding) -> folding -> HashDict key value -> folding
foldl step folding = .trie >> HashTrie.foldl step folding

{-|
Convert a dictionary into an association list of key-value pairs.
-}
toList : HashDict key value -> List (key, value)
toList = .trie >> HashTrie.toList
