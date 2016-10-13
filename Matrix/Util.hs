-- ***Utilities***
module Matrix.Util
( ListMatrix(..)
, nyi
, err
) where


--- **Type Synomyms**

---- Treat lists of lists as Matrices
type ListMatrix a = [[a]]


--- **Utility Functions**

---- Errors with a NYI message and the given context
nyi::String -> null
nyi context = err ("Feature Not yet Implemented! Context: " ++ context)

---- Errors with the provided message
err::String -> null
err msg = error ("ERROR: " ++ msg)
