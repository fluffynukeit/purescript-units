# Module Documentation

## Module Data.Units.Core

### Types

     Dimension wraps an arbitrary record describing a set of orthogonal dimensions.
     e.g. length, time, mass.  All fields must be Number types.

    newtype Dimension b where
      Dimension :: {  | b } -> Dimension b

     Record of the operation error.  Contains the operation error type, left
     dimension, right dimension, and optional user context.

    data QError b c where
      QError :: QErrorType -> Maybe c -> Dimension b -> Dimension b -> QError b c

     Types describing invalid operations between Quantities.

    data QErrorType where
      QAdd :: QErrorType
      QSubtract :: QErrorType
      QModulo :: QErrorType
      QUnits :: QErrorType

     Quantity is a Num instance value paired with a Dimension.  Basic math
     operations can be done between Quantities and resulting Dimensions are 
     tracked.  Illegal operations are described by QuantityError.  Operations on 
     one or two QuantityErrors propagate the errors.

    data Quantity a b c where
      Quantity :: a -> Dimension b -> Quantity a b c
      QuantityError :: [QError b c] -> Quantity a b c


### Type Class Instances


    instance eqDimension :: Eq (Dimension b)


    instance quantEq :: (Eq a) => Eq (Quantity a b c)


    instance quantNum :: (Num a) => Num (Quantity a b c)


    instance showDim :: Show (Dimension b)


    instance showQError :: (Show c) => Show (QError b c)


### Values

     Multiply two Dimensions.

    (**) :: forall b. Dimension b -> Dimension b -> Dimension b

     Multiply dimensionless Num on the right side.

    (*.) :: forall a b c. (Num a) => Quantity a b c -> a -> Quantity a b c

     Multiply dimensionless Num on the left side.

    (.*) :: forall a b c. (Num a) => a -> Quantity a b c -> Quantity a b c

     Divide dimensionless Num by a Quantity.

    (./) :: forall a b c. (Num a) => a -> Quantity a b c -> Quantity a b c

     Divide a Quantity by a dimensionless Num.

    (/.) :: forall a b c. (Num a) => Quantity a b c -> a -> Quantity a b c

     Divide two Dimensions.

    (//) :: forall b. Dimension b -> Dimension b -> Dimension b

     Functionally the same as (.*) but with highest operator precedence. Useful
     for tagging Nums with Quantities respresenting units without needing to use 
     parentheses.  e.g. 30 @+ km / 10 @+ m/s instead of (30 .* km) / (10 .* m/s)

    (@+) :: forall a b c. (Num a) => a -> Quantity a b c -> Quantity a b c

     Divide one Quantity by another, but return a QuantityError if the Dimensions
     are inconsistent.  Useful for unit conversion.  e.g. 10 @ m/s @- kph.  Can
     be thought of as "subtracting" the dimension off a Quantity.

    (@-) :: forall a b c. (Num a) => Quantity a b c -> Quantity a b c -> Either [QError b c] a

     Raise a quantity to a Number power.

    (^.) :: forall b c. Quantity Number b c -> Number -> Quantity Number b c

     Raise a dimension to a Number power.

    (^^) :: forall b. Dimension b -> Number -> Dimension b

     Extract the Dimension from a Quantity.  Returns the QuantityError's
     [QError] if the Quantity is a QuantityError.

    dim :: forall a b c. Quantity a b c -> Either [QError b c] (Dimension b)

     Convert a Dimension to a compatible dimensionless Dimension.
     for all d, dim1 d == d ^^ 0

    dim1 :: forall b. Dimension b -> Dimension b

     Inject context into the Quantity calculation chain.  If the input is Quantity,
     it is return unchanged.  If the input is a QuantityError with the top error
     having a context already assigned, the QuantityError is return unchanged.  
     Otherwise, the top error in the error list is assigned the input context.
     Useful for narrowing down where a calculation failed.

    qCtx :: forall a b c. c -> Quantity a b c -> Quantity a b c

     Raise a Number Quantity to the reciprocal power. e.g. root 2 == sqrt

    root :: forall b c. Number -> Quantity Number b c -> Quantity Number b c

     Utility function for defining fundamental units.  Creates a Quantity with
     the given Dimension and value 1 of Number. e.g. meter = std dLength

    std :: forall b c. Dimension b -> Quantity Number b c


## Module Data.Units.Physical.SI


