module HashingContainers.HashSet exposing (..)

import Array exposing (Array)
import Typeclasses.Classes.Equality exposing (Equality)
import Typeclasses.Classes.Hashing exposing (Hashing)


type alias HashSet entry =
  {
    equality : Equality entry,
    hashing : Hashing entry,
    data : Array (List entry)
  }

empty : Equality entry -> Hashing entry -> Int -> HashSet entry
empty equality hashing sizeAsPowerOfTwo = Debug.todo ""

fromList : Equality entry -> Hashing entry -> Int -> List entry -> HashSet entry
fromList = Debug.todo ""
