-- ***Vector***
module MathUtil.Vector
( 
{- DataTypes -}
  Vector ()
{- Factory Functions -}
, makeVectorFiller
, makeVectorLinkedList
, listToVector
, vectorToList
, makeZeroVector
, makeUnitVector
, makeAxisUnitVector2D
, makeAxisUnitVector3D
{- Methods -}
, leng
, lengSquared
{- Misc Functions -}
) where


--- **Imports**
import qualified MathUtil.Util as Util
import MathUtil.LinkedList


--- **DataType Declarations**

---- Vector: a Vector of Doubles
data Vector = Vector (LinkedList Double) deriving (Eq, Ord, Show, Read)


--- **DataType Factory Functions**

---- Creates a Vector, with param 1 as the number of components, and param 2 as the default value
----   Returns an error String instead if param 1 is <= 0
makeVectorFiller:: Int -> Double -> Either String Vector
makeVectorFiller 0 _ = Left $ getErrMsg 10 "makeVectorFiller"
makeVectorFiller dim val 
            | dim < 0 = Left $ getErrMsg 11 "makeVectorFiller"
            | otherwise = Right (Vector (makeLinkedListFiller dim val))

---- Converts a LinkedList into a Vector
----   Returns an error String instead if param 1 is an EmptyList
makeVectorLinkedList:: (LinkedList Double) -> Either String Vector
makeVectorLinkedList (EmptyList) = Left $ getErrMsg 10 "makeVectorLinkedList"
makeVectorLinkedList llist = Right (Vector llist)

---- Converts a List of Doubles into a Vector
----   Returns an error String instead if param 1 is []
listToVector:: [Double] -> Either String Vector
listToVector [] = Left $ getErrMsg 10 "listToVector"
listToVector list = Right (Vector (listToLinkedList list))

---- Converts a Vector into a List of Doubles
vectorToList:: Vector -> [Double]
vectorToList (Vector v) = Vector (vectorToListInternal v)
            where vectorToListInternal:: (LinkedList Double)-> [Double]
                  vectorToListInternal (EmptyList) = []
                  vectorToListInternal (Node v vs) = v : (vectorToList vs)
                  
---- Shortcut for: makeVectorFiller (param 1) 0
----   See makeVectorFiller
makeZeroVector:: Int -> Either String Vector
makeZeroVector dim = (makeVectorFiller dim 0)

---- Makes a Unit Vector pointing in direction of param 1
makeUnitVector:: Vector -> Vector
makeUnitVector (Vector v) = Vector (makeUnitVectorInternal v (leng v))
            where makeUnitVectorInternal:: (LinkedList Double) -> Double -> (LinkedList Double)
                  makeUnitVectorInternal (EmptyList) _ = EmptyList
                  makeUnitVectorInternal (Node v vs) l = makeNodeT (v * (fromRantional (1 % l))) (makeUnitVectorInternal vs)

---- Constructs a 2-D unit vector with direction in axis param 1 (x = 0, y = 1)
makeAxisUnitVector2D:: Int -> Vector
makeAxisUnitVector2D 0 = Right $ Vector makeNodeT 1 makeNode 0
makeAxisUnitVector2D 1 = Right $ Vector makeNodeT 0 makeNode 1
makeAxisUnitVector2D x = Left $ getErrMsg 12 "makeAxisUnitVector2D"

---- Constructs a 3-D unit vector with direction in axis param 1 (x = 0, y = 1, z = 2)
makeAxisUnitVector3D:: Int -> Vector
makeAxisUnitVector3D 0 = Right $ Vector makeNodeT 1 makeNodeT 0 makeNode 0
makeAxisUnitVector3D 1 = Right $ Vector makeNodeT 0 makeNodeT 1 makeNode 0
makeAxisUnitVector3D 2 = Right $ Vector makeNodeT 0 makeNodeT 0 makeNode 1
makeAxisUnitVector3D x = Left $ getErrMsg 12 "makeAxisUnitVector3D"


--- **DataType Methods**

---- Returns the length(magnitude) of the vector
leng:: Vector -> Double
leng v = sqrt (lengSquared v)

---- Returns the length(magnitude) squared of the vector
----   NOTE: this function is faster than calling `(leng v)^2', and thus should be prefered where applicable
lengSquared:: Vector -> Double
lengSquared (Vector v) = lengSquaredInternal v
            where lengSquaredInternal:: (LinkedList Double) -> Double
                  lengSquaredInternal (EmptyList) = 0.0
                  lengSquaredInternal (Node v vs) = (v^2) + (lengSquaredInternal vs)


--- **Misc Functions**

---- Returns an error message from given a error code
getErrMsg:: Int -> String -> String
getErrMsg 10 context = (getErrPrefix context) ++ "Vector must have at least one component/dimension!"
getErrMsg 11 context = (getErrPrefix context) ++ "Vector cannot have a negative dimension/number of components!"
getErrMsg 12 context = (getErrPrefix context) ++ "Axis number out of bounds!!"

getErrPrefix:: String -> String
getErrPrefix context = "[Matrix/Vectorhs: " ++ context ++ "]"