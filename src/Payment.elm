module Payment exposing (..)

import String
import List as L
import Util

type alias Payment = { value : Float, category: String, description: String }

defaultPayment = { value = 0.0, category = "food", description = "default" }

createPayment : String -> Payment
createPayment str = let parts = String.split "," str
                    in case parts of
                      []                  -> defaultPayment
                      _ :: []             -> defaultPayment
                      _ :: _ :: []        -> defaultPayment
                      (val::cat::desc::_) -> {
                                               value       = Util.toFloatSure val,
                                               category    = cat,
                                               description = desc
                                             }
  
totalPaid : List Payment -> Float
totalPaid payments = L.sum (L.map .value payments)

-- Tests for Payment

testCreatePayment : String -> Payment -> Bool
testCreatePayment str payment = (createPayment str) == payment

testTotalPaid : List Payment -> Float -> Bool
testTotalPaid payments expectedPaid = (totalPaid payments) == expectedPaid

samplePayments : List Payment
samplePayments = [
                   { value = 10.0, category = "food"           , description = "lunch"},
                   { value = 25.0, category = "trannsportation", description = "uber"}
                 ]
