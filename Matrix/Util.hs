-- ***Utilities***
module Matrix.Util
(
{- Type Synomyms -}
  ListMatrix(..)
{- Functions -}
, getMaybeVal
, getRightVal
, nyi
, err
) where


--- **Imports**


--- **Type Synomyms**

---- Treat lists of lists as Matrices
type ListMatrix a = [[a]]


--- **Utility Functions**

---- Gets the Value, v, out of a `Just v' or param 2 if param 1 is of type `Nothing'
getMaybeVal:: (Maybe a) -> a -> a
getMaybeVal (Just v) _ = v
getMaybeVal (Nothing) d = d

---- Gets the Value, v, out of a `Right v' or errors with String, s, from `Left s' 
getRightVal:: (Either String a) -> a
getRightVal (Left s) = err s
getRightVal (Right v) = v

---- Errors with a NYI message and the given context
nyi::String -> null
nyi context = err ("Feature Not yet Implemented! Context: " ++ context)

---- Errors with the provided message
err::String -> null
err msg = error ("ERROR: " ++ msg)
