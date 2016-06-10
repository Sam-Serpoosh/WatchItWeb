module Payment
  exposing
    ( Payment
    , createPayment
    , createPayments
    , totalPaid
    , paymentsByCategory
    , paidPerCategory
    , percentPaidPerCategory
    )

{-| This module does some basic calculations and stats
on a list of Payments and return some reports.

# Data Types
@docs Payment

# Creation of Payment
@docs createPayment, createPayments

# Calculations
@docs totalPaid, paymentsByCategory, paidPerCategory, percentPaidPerCategory

-}

import String     as S
import List       as L
import List.Extra as LE
import Util

{-| Payment Record with 3 fields:
      - $ value
      - category it belongs to (e.g food)
      - description of payment (e.g lunch with Bob)

-}

type alias Payment = { value : Float
                     , category: String
                     , description: String
                     }

{-| create a Payment from a list of CSV Strings.

    createPayments ["10.00,food,lunch", "25.00,transportation,uber"] == [p1, p2]
-}

createPayments : List String -> List Payment
createPayments paymentsStr = L.map createPayment paymentsStr


{-| Calculates total amound paid.

    totalPaid [p1-10, p2-5] == 15
-}

totalPaid : List Payment -> Float
totalPaid payments =
  L.sum (L.map .value payments)

{-| categorize payments by their category value.

    paymentsByCategory [p1-food, p2-transportation, p3-food] == [("food", [p1, p3]), ("transportation", [p2])]
-}

paymentsByCategory : List Payment -> List (String, List Payment)
paymentsByCategory payments =
  let grouped = LE.groupWhile (\p1 p2 -> p1.category == p2.category) (L.sortBy .category payments)
  in L.map (\ps -> (ps |> Util.ensureHead defaultPayment |> .category, ps)) grouped

{-| Calculates total amound paid per category.

    totalPaid [("food", [p1, p3]), ("transportation", [p2])] == [("food", 30), ("transportation", 5)]
-}

paidPerCategory : List (String, List Payment) -> List (String, Float)
paidPerCategory = L.map (\(cat, payments) -> (cat, payments |> totalPaid))

{-| Calculates percent of total paid for each category.

    totalPaid [("food", 30), ("transportation", 5)] == [("food", 0.375), ("transportation", 0.625)]
-}

percentPaidPerCategory : List (String, Float) -> List (String, Float)
percentPaidPerCategory paidPerCat =
  let total = L.foldl (\a b -> a + b) 0.0 (L.map snd paidPerCat)
  in L.map (\(cat, paid) -> (cat, paid / total)) paidPerCat

{-| create a Payment from a CSV Strings.

    createPayments "10.00,food,lunch == p1
-}

createPayment : String -> Payment
createPayment str =
  let parts = S.split "," str
  in case parts of
    []                  -> defaultPayment
    _ :: []             -> defaultPayment
    _ :: _ :: []        -> defaultPayment
    (val::cat::desc::_) -> { value       = Util.ensureToFloat val
                           , category    = cat
                           , description = desc
                           }

defaultPayment : Payment
defaultPayment = { value       = 0.0
                 , category    = "food"
                 , description = "default"
                 }
