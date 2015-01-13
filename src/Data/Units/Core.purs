
module Data.Units.Core 
  ( Dimension(..)
  , (**), (//), (^^)
  , dim1
  , Quantity(..)
  , QError(..)
  , QErrorType(..)
  , dim, std, (@+), (@-)
  , (.*), (*.), (./), (/.), (^.), root
  , qCtx
  )
where

import Data.Function
import Data.Array
import Data.Either
import Data.Maybe

-- Dimension wraps an arbitrary record describing a set of orthogonal dimensions.
-- e.g. length, time, mass.  All fields must be Number types.
newtype Dimension b = Dimension {|b}

foreign import dimEq """
  var dimEq = function(d1, d2) {
    for (var f in d1) {
      if (d1.hasOwnProperty(f) && d1[f] !== d2[f]) {
         return false;
      }
    }
    return true;
  };""" :: forall a. Fn2 {|a} {|a} Boolean

instance eqDimension :: Eq (Dimension b) where
  (==) (Dimension a) (Dimension b) = runFn2 dimEq a b
  (/=) a b                         = not $ a == b

foreign import dimUpdate """
  var dimUpdate = function(op, d1, d2) {
    var d3 = {};
    for (var f in d1) {
      if (d1.hasOwnProperty(f)) {
         d3[f] = op(d1[f], d2[f]);
      }
    }
    return d3;
  }; """ :: forall a. Fn3 
                      (Fn2 Number Number Number) 
                      {|a}
                      {|a}
                      {|a}

-- Multiply two Dimensions.
(**) :: forall b. Dimension b -> Dimension b -> Dimension b
(**) (Dimension a) (Dimension b) = Dimension $ runFn3 dimUpdate (mkFn2 (+)) a b

-- Divide two Dimensions.
(//) :: forall b. Dimension b -> Dimension b -> Dimension b
(//) (Dimension a) (Dimension b) = Dimension $ runFn3 dimUpdate (mkFn2 (-)) a b

foreign import dimExp """
  var dimExp = function(n, d1) {
    var d2 = {};
    for (var f in d1) {
      if (d1.hasOwnProperty(f)) {
         d2[f] = d1[f] * n;
      }
    }
    return d2;
  }; """ :: forall a. Fn2
                      Number
                      {|a}
                      {|a}

-- Raise a dimension to a Number power.
(^^) :: forall b. Dimension b -> Number -> Dimension b
(^^) (Dimension d) n = Dimension $ runFn2 dimExp n d

infixl 8 ^^
infixl 7 **
infixl 7 //

-- Convert a Dimension to a compatible dimensionless Dimension.
-- for all d, dim1 d == d ^^ 0
dim1 :: forall b. Dimension b -> Dimension b
dim1 = flip (^^) 0

foreign import dimDescribe """
  var dimDescribe = function(d1) {
    var sNum = "";
    var sDen = "";
    for (var f in d1) {
      if (d1.hasOwnProperty(f)) {
         var val = d1[f];
         if      (val > 0) { sNum = sNum + f + val + " ";}
         else if (val < 0) { sDen = sDen + f + (-val) + " ";}
      }
    }
    var reduce = function (s, dfl) {
      if (s.length == 0) { s = dfl; } 
      else               { s = s.substring(0, s.length-1); }
      return s;
    }

    sNum = reduce(sNum, "1");
    sDen = reduce(sDen, "");

    return (sDen.length == 0) ? sNum : sNum + "/" + sDen;
  }; """ :: forall a. {|a} -> String

instance showDim :: Show (Dimension b) where
  show (Dimension a) = dimDescribe a



-- Quantity is a Num instance value paired with a Dimension.  Basic math
-- operations can be done between Quantities and resulting Dimensions are 
-- tracked.  Illegal operations are described by QuantityError.  Operations on 
-- one or two QuantityErrors propagate the errors.
data Quantity a b c = Quantity a (Dimension b)
                    | QuantityError [QError b c]

-- Record of the operation error.  Contains the operation error type, left
-- dimension, right dimension, and optional user context.
data QError b c = QError QErrorType (Maybe c) (Dimension b) (Dimension b)

-- Types describing invalid operations between Quantities.
data QErrorType = QAdd | QSubtract | QModulo | QUnits

prependCtx :: forall a. (Show a) => Maybe a -> String -> String
prependCtx ctx msg = maybe msg (show >>> flip (++) (": " ++ msg)) ctx

instance showQError :: (Show c) => Show (QError b c) where
  show (QError QAdd ctx d1 d2) = 
    prependCtx ctx $ "Cannot add quantity with dimension [" ++ show d1 ++ "] to quantity with dimension [" ++ show d2 ++ "]."
  show (QError QSubtract ctx d1 d2) = 
    prependCtx ctx $ "Cannot subtract quantity with dimension [" ++ show d1 ++ "] from quantity with dimension [" ++ show d2 ++ "]."
  show (QError QModulo ctx d1 d2) =
    prependCtx ctx $ "Cannot modulo quantity with dimension [" ++ show d1 ++ "] by quantity with dimension [" ++ show d2 ++ "]."
  show (QError QUnits ctx d1 d2) = 
    prependCtx ctx $ "Cannot unify dimension [" ++ show d1 ++ "] with dimension [" ++ show d2 ++ "]."

-- Extract the Dimension from a Quantity.  Returns the QuantityError's
-- [QError] if the Quantity is a QuantityError.
dim :: forall a b c. Quantity a b c -> Either [QError b c] (Dimension b)
dim (Quantity _ d)    = Right d
dim (QuantityError s) = Left s


-- Functionally the same as (.*) but with highest operator precedence. Useful
-- for tagging Nums with Quantities respresenting units without needing to use 
-- parentheses.  e.g. 30 @+ km / 10 @+ m/s instead of (30 .* km) / (10 .* m/s)
(@+) :: forall a b c. (Num a) => a -> Quantity a b c -> Quantity a b c
(@+) = (.*)

-- Divide one Quantity by another, but return a QuantityError if the Dimensions
-- are inconsistent.  Useful for unit conversion.  e.g. 10 @ m/s @- kph.  Can
-- be thought of as "subtracting" the dimension off a Quantity.
(@-) :: forall a b c. (Num a) => Quantity a b c -> Quantity a b c -> Either [QError b c] a
(@-) q1 q2 = case quantOp QUnits (/) q1 q2 of
                  Quantity a _    -> Right a
                  QuantityError s -> Left s

infixr 9 @+
infixr 6 @-


-- Multiply dimensionless Num on the left side.
(.*) :: forall a b c. (Num a) => a -> Quantity a b c -> Quantity a b c
(.*) n (Quantity v d) = Quantity (n*v) d
(.*) n err = err

-- Multiply dimensionless Num on the right side.
(*.) :: forall a b c. (Num a) => Quantity a b c -> a -> Quantity a b c
(*.) = flip (.*)

-- Divide dimensionless Num by a Quantity.
(./) :: forall a b c. (Num a) => a -> Quantity a b c -> Quantity a b c
(./) n (Quantity v d) = Quantity (n/v) (d ^^ -1)
(./) n err = err

-- Divide a Quantity by a dimensionless Num.
(/.) :: forall a b c. (Num a) => Quantity a b c -> a -> Quantity a b c
(/.) (Quantity v d) n = Quantity (v/n) d
(/.) err _ = err


foreign import expImpl' """
  var expImpl$prime = Math.pow;
  """ :: Fn2 Number Number Number

-- Raise a quantity to a Number power.
(^.) :: forall b c. Quantity Number b c -> Number -> Quantity Number b c
(^.) (Quantity n d) e = Quantity (runFn2 expImpl' n e) (d ^^ e)
(^.) err _ = err

-- Raise a Number Quantity to the reciprocal power. e.g. root 2 == sqrt
root :: forall b c. Number -> Quantity Number b c -> Quantity Number b c
root n = flip (^.) (1/n)

infixl 8 ^.
infixl 7 *.
infixl 7 .*
infixl 7 ./
infixl 7 /.


-- Utility function for defining fundamental units.  Creates a Quantity with
-- the given Dimension and value 1 of Number. e.g. meter = std dLength
std :: forall b c. Dimension b -> Quantity Number b c
std d = Quantity 1 d




-- Perform an operation, but return an error if dimensions are inconsistent.
quantOp :: forall a b c. 
           QErrorType -> 
           (a -> a -> a) -> 
           Quantity a b c -> 
           Quantity a b c -> 
           Quantity a b c
quantOp _ _ (QuantityError s1)     (QuantityError s2)    = QuantityError (s2 ++ s1)
quantOp _ _ err@(QuantityError _)  _                     = err
quantOp _ _ _                      err@(QuantityError _) = err
quantOp s o (Quantity v1 d1)       (Quantity v2 d2)      
  | d1 == d2  = Quantity (o v1 v2) d1
  | otherwise = QuantityError [QError s Nothing d1 d2]

-- Perform an operation on both the values and the dimensions.
quantMult :: forall a b c. 
             (a -> a -> a) -> 
             (Dimension b -> Dimension b -> Dimension b) -> 
             Quantity a b c -> 
             Quantity a b c -> 
             Quantity a b c
quantMult _   _   (QuantityError s1)    (QuantityError s2)    = QuantityError (s2 ++ s1)
quantMult _   _   err@(QuantityError _) _                     = err
quantMult _   _   _                     err@(QuantityError _) = err
quantMult vOp dOp (Quantity v1 d1)      (Quantity v2 d2)      = Quantity (vOp v1 v2) (dOp d1 d2)

instance quantNum :: (Num a) => Num (Quantity a b c) where
  (+) = quantOp QAdd (+)
  (-) = quantOp QSubtract (-)
  (%) = quantOp QModulo (%)
  (*) = quantMult (*) (**)
  (/) = quantMult (/) (//)
  negate (Quantity v d) = Quantity (negate v) d
  negate err            = err


instance quantEq :: (Eq a) => Eq (Quantity a b c) where
  (==) (Quantity v1 d1)   (Quantity v2 d2) = v1 == v2 && d1 == d2
  (==) _                  _                = false

  (/=) q1 q2 = not $ q1 == q2

-- Inject context into the Quantity calculation chain.  If the input is Quantity,
-- it is return unchanged.  If the input is a QuantityError with the top error
-- having a context already assigned, the QuantityError is return unchanged.  
-- Otherwise, the top error in the error list is assigned the input context.
-- Useful for narrowing down where a calculation failed.
qCtx :: forall a b c. c -> Quantity a b c -> Quantity a b c
qCtx c (QuantityError ((QError t Nothing d1 d2):errors)) = 
  QuantityError ((QError t (Just c) d1 d2):errors)
qCtx _ q = q
