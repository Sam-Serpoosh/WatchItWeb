module Payment exposing (..)

import String     as S
import List       as L
import List.Extra as LE
import Util

type alias Payment = { value : Float
                     , category: String
                     , description: String
                     }

defaultPayment : Payment
defaultPayment = { value       = 0.0
                 , category    = "food"
                 , description = "default" 
                 }

createPayments : List String -> List Payment
createPayments paymentsStr = L.map createPayment paymentsStr

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
  
totalPaid : List Payment -> Float
totalPaid payments =
  L.sum (L.map .value payments)

paymentsByCategory : List Payment -> List (String, List Payment)
paymentsByCategory payments =
  let grouped = LE.groupWhile (\p1 p2 -> p1.category == p2.category) (L.sortBy .category payments)
  in L.map (\ps -> (ps |> Util.ensureHead defaultPayment |> .category, ps)) grouped

paidPerCategory : List (String, List Payment) -> List (String, Float)
paidPerCategory = L.map (\(cat, payments) -> (cat, payments |> totalPaid))

percentPaidPerCategory : List (String, Float) -> List (String, Float)
percentPaidPerCategory paidPerCat =
  let total = L.foldl (\a b -> a + b) 0.0 (L.map snd paidPerCat)
  in L.map (\(cat, paid) -> (cat, paid / total)) paidPerCat

-- Tests for Payment

testCreatePayment : String -> Payment -> Bool
testCreatePayment str payment =
  (createPayment str) == payment

testTotalPaid : List Payment -> Float -> Bool
testTotalPaid payments expectedPaid =
  (totalPaid payments) == expectedPaid

testPaymentsByCategory : List Payment -> List (String, List Payment) -> Bool
testPaymentsByCategory payments expectedGrouped =
  (paymentsByCategory payments) == expectedGrouped

testPaidPerCategory : List (String, List Payment) -> List (String, Float) -> Bool
testPaidPerCategory paymentsByCat expectedPaidPerCat =
  (paidPerCategory paymentsByCat) == expectedPaidPerCat

testPercentPaidPerCategory : List (String, Float) -> List (String, Float) -> Bool
testPercentPaidPerCategory paidPerCat expectedPercents =
  (percentPaidPerCategory paidPerCat) == expectedPercents

expectedGrouped : List (String, List Payment)
expectedGrouped = [("food", [lunch, snack]), ("transportation", [uber])]

samplePayments : List Payment
samplePayments = [lunch, uber, snack]

samplePaidPerCat : List (String, Float)
samplePaidPerCat = [("food", 15.0), ("transportation", 25.0)]

lunch : Payment
lunch = { value       = 10.0
        , category    = "food"
        , description = "lunch"
        }

snack : Payment
snack  = { value = 5.0
         , category = "food"
         , description = "snack"
         }

uber : Payment
uber = { value    = 25.0
       , category = "transportation"
       , description = "uber"
       }
