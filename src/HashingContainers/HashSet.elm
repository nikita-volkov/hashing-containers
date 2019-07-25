module HashingContainers.HashSet exposing
  (
    HashSet,
    empty,
    fromList,
    fromArray,
    insert,
    remove,
    member,
    isEmpty,
    foldl,
    toList
  )
{-|
HashSet API.

@docs HashSet

# Construction
@docs empty, fromList, fromArray

# Transformation
@docs insert, remove

# Access
@docs member, isEmpty, foldl, toList
-}

import Array exposing (Array)
import Typeclasses.Classes.Equality as Equality exposing (Equality)
import Typeclasses.Classes.Hashing as Hashing exposing (Hashing)
import HashingContainers.HashTrie as HashTrie exposing (HashTrie)
import HashingContainers.Extensions.Maybe as Maybe


{-|
Generic set of hashable elements.

Unlike `Set` from the "core" library it is not
limited to any specific set of supported types for elements.
-}
type alias HashSet value =
  {
    equality : Equality value,
    hashing : Hashing value,
    trie : HashTrie value
  }

{-|
Construct an empty HashSet,
providing instances of
[`Equality`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Equality#Equality)
and
[`Hashing`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Hashing#Hashing)
explicit typeclasses for value.
-}
empty : Equality value -> Hashing value -> HashSet value
empty equality hashing = HashSet equality hashing HashTrie.empty

fromFoldable :
  ((value -> HashTrie value -> HashTrie value) -> HashTrie value -> foldable -> HashTrie value) ->
  Equality value -> Hashing value ->
  foldable -> HashSet value
fromFoldable fold equality hashing foldable =
  HashSet equality hashing
    (HashTrie.fromFoldable fold hashing.hash equality.eq foldable)

{-|
Construct from a list of values,
providing instances of
[`Equality`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Equality#Equality)
and
[`Hashing`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Hashing#Hashing)
explicit typeclasses for value.
-}
fromList : Equality value -> Hashing value -> List value -> HashSet value
fromList = fromFoldable List.foldl

{-|
Construct from an array of values,
providing instances of
[`Equality`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Equality#Equality)
and
[`Hashing`](/packages/nikita-volkov/typeclasses/1.5.0/Typeclasses-Classes-Hashing#Hashing)
explicit typeclasses for value.
-}
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

{-|
Insert a value into a set.
-}
insert : value -> HashSet value -> HashSet value
insert value = mapHashTrieAtKey (HashTrie.insert value) value

{-|
Remove a value from a set. If the value is not found, no changes are made.
-}
remove : value -> HashSet value -> HashSet value
remove = mapHashTrieAtKey HashTrie.remove

{-|
Determine if a value is in a set.
-}
member : value -> HashSet value -> Bool
member value = accessHashTrieAtKey HashTrie.get value >> Maybe.match False (always True)

{-|
Determine if a set is empty.
-}
isEmpty : HashSet value -> Bool
isEmpty = .trie >> HashTrie.isEmpty

{-|
Fold over the values in a set.
-}
foldl : (value -> folding -> folding) -> folding -> HashSet value -> folding
foldl step folding = .trie >> HashTrie.foldl step folding

{-|
Convert a set into a list.
-}
toList : HashSet value -> List value
toList = .trie >> HashTrie.toList
