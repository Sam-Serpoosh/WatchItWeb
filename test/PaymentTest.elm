module PaymentTest exposing (..)

{-| This module has some ad-hoc test functions
for verifying the functionality of Payment module.

elm-test setup and running suites proved to be a 
pain in the neck due to its immaturity so I just
wrote some simple test functions here.

-}

import Payment exposing (..)
import String

-- Sample Data

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

expectedGrouped : List (String, List Payment)
expectedGrouped = [("food", [lunch, snack]), ("transportation", [uber])]

expectedPaidPerCat : List (String, Float)
expectedPaidPerCat = [("food", 15.0), ("transportation", 25.0)]

expectedPercentPerCat : List (String, Float) 
expectedPercentPerCat = [("food", 0.375), ("transportation", 0.625)]

testCreatePayment : String -> Payment -> Bool
testCreatePayment str payment =
  (createPayment str) == payment

testTotalPaid : List Payment -> Float -> Bool
testTotalPaid payments expectedPaid =
  (totalPaid payments) == expectedPaid

testPaymentsByCategory : List Payment -> List (String, List Payment) -> Bool
testPaymentsByCategory payments expectedG =
  (paymentsByCategory payments) == expectedG

testPaidPerCategory : List (String, List Payment) -> List (String, Float) -> Bool
testPaidPerCategory paymentsByCat expectedPaidPerCat =
  (paidPerCategory paymentsByCat) == expectedPaidPerCat

testPercentPaidPerCategory : List (String, Float) -> List (String, Float) -> Bool
testPercentPaidPerCategory paidPerCat expectedPercents =
  (percentPaidPerCategory paidPerCat) == expectedPercents

runAllTests : List (String, Bool)
runAllTests =
  let creationTest      = ("createPayment",          testCreatePayment "10.0,food,lunch" lunch)
      totalPaidTest     = ("totalPaid",              testTotalPaid samplePayments 40.0)
      byCatTest         = ("paymentsByCategory",     testPaymentsByCategory samplePayments expectedGrouped)
      paidPerCatTest    = ("paidPerCategory",        testPaidPerCategory expectedGrouped expectedPaidPerCat)
      percentPerCatTest = ("percentPaidPerCategory", testPercentPaidPerCategory expectedPaidPerCat expectedPercentPerCat)
  in [creationTest, totalPaidTest, byCatTest, paidPerCatTest, percentPerCatTest]

