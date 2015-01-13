# purescript-units
Purescript types and functions for managing units of measure at runtime.

The API and implementation are currently experimental, subject to change, and
open to suggestion.

### Motivation

Adding 4 meters to 2 seconds doesn't make sense but is easy to do by mistake when
both quantities are represented by simple `Number`s.  For correct 
implementation, understanding both a value and its significance is essential.

### Functionality

This library provides functionality for performing mathematical operations and 
tracking the dimensionality of results along the way.  Any operation that does
not make sense dimensionally returns an error type that is propagated through
subsequent operations.  Conversion between units of the same dimensionality is 
easy.

This library provides two pieces of functionality.

1. `Data.Units.Core` defines types and functions for defining `Dimensions` and
`Quantities` that represent values in those `Dimensions`.  For example, time is
a `Dimension`, and 1 second is a `Quantity` of time.  The functionality included
in this module is general and allows the user to define his or her own dimension
sets, do math and unit conversion with them, and examine results. For example,
possible dimensions could include sales, points, coverage area, reviews, etc.
2. `Data.Units.Physical.SI` uses the functionalty in `Data.Units.Core` to 
define the SI set of dimensions (length, time, amount, current, mass, 
temperature, and luminous intensity) as well as the standard units of each
(meter, second, mol, etc.).

Future additions include more physical units and possibly additional dimension
standards.

### Usage

Example usage is provided in the `test` directory.
