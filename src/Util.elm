module Util exposing (..)

import String as S

toFloatSure : String -> Float
toFloatSure str = let f = S.toFloat str
                  in case f of
                    Ok num -> num
                    Err _  -> 0.0
