module Util exposing (..)

import String as S
import List   as L

ensureToFloat : String -> Float
ensureToFloat str =
  let f = S.toFloat str
  in case f of
    Ok num -> num
    Err _  -> 0.0

ensureHead : a -> List a -> a
ensureHead default items =
  let head = items |> L.head
  in case head of
    Just elem -> elem
    Nothing   -> default
