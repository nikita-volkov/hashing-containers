# Summary

Hashing-based container data-structures, which unlike the standard `Set` and `Dict` are not limited to just comparable values.

# Motivation

## Containers from "core" are not generic

Elm has a rough edge in its design when it comes to generic operations:
the compiler provides several functions which magically generalize over numeric, comparable and appendable types
(for details see [the "Constrained type variables" section of Elm Guide](https://guide.elm-lang.org/types/reading_types.html#constrained-type-variables)). Because the implementations of `Dict` and `Set` in the "core" package are limited to just the types satisfying the `comparable` magical constraint, you only get `Int`, `Float`, `Char`, `String`, and lists/tuples to be used for keys. You cannot have custom types there.

## Association lists have terrible performance

A naive workaround to the limitation of `Dict` is to implement an association list: a list of tuples of keys and values, and search by traversing, but that comes with terrible performance characteristics.

## Hashing-based algorithms outperform the comparison-based ones

Besides the mentioned issues due to the differences of algorithms hashing-based containers outperform comparison-based ones in most use-cases. Especially this comes to light, when the key is not a primitive value. More on this in [the Performance section](#performance). So even if not for the other issues, this one alone is enough to have such data-structures.

# Performance

Have you ever thought how comparison on `String` works? You iterate through each character of both compared strings until you hit the pair that is not equal or one of the strings ends. Same goes for composite datastructures: tuples, records, ADTs, arrays. You basically traverse through them until you hit something that is not equal. IOW, this operation is linear or has a complexity of `O(n)`.

Imagine having a String for key in `Dict` of "core". On every lookup you'll be executing such comparison operation multiple times.

Now let's consider non-cryptographic hashing. It is a function which maps values of your type to integers, providing only for two properties: equal values of your type must produce equal integers, and different values must be likely to produce different integers.

A good enough implementation of a hashing function for strings of any length can be sampling just three characters: somewhere from the beginning, from the middle and from the end. Characters can easily be converted to ints by getting their indices in the Unicode table, which is what the `Char.toCode` operation does. This means that you can represent any string with these three integers. Sprinkle it with another integer produced from the string's length and you have embedded your string versatilely in 4 integers. All that's left is just to combine these 4 integers into one, which I'm sure you know multiple simple ways to. And that's it, these few primitive operations are all you need to get a hash.

Hashtables are implemented in such a way that any search operation is most likely to only cause one such hashing of the key and some very fast lookup in an integer table.

This all about the nature of the different performance characteristics. What we're lacking for now is the benchmarks, so a contribution on that part is welcome.

# Implementation

The implementation of the datastructure is in many ways inspired by the HAMT-based hash-maps in other languages like Haskell, Clojure and Scala. However for now it is simpler: instead of implementing a trie, we're reusing an existing implementation of `IntDict` as a map between key hashes and buckets.

Although not proven yet, performance-wise such implementation may be inferior in comparison to full-on HAMT, which is why the implementation may change in the future. OTOH for now Elm unfortunately doesn't expose the native JS array, which limits our abilities in implementing performant HAMT. Regardless, any changes to the datastructure implementation should have no effect on the API.

# Status

The API has only the basic functionality implemented for now and has the critical parts covered with tests. Contribution is welcome.
