-- ***ListMatrix Operations***
module MathUtil.Operations
(
{- Row Functions -}
  rSwap
, rScale
, rAdd
, rSub
{- Matrix Functions -}
, mScale
, mAdd
, mSub
, transpose
, trace
, determinant
{- Misc Functions -}
, numRows
, numCols
, checkJagged
, isSquare
) where


--- **Imports**
import qualified Data.List as LST -- for transpose
import qualified MathUtil.Util as Util


--- **Row Functions**

---- Swaps two rows and returns them in a tuple: r1 r2 -> (r2, r1)
----   NOTE: the VALUES in the arrays are NEITHER reversed NOR swapped
rSwap::(Num a) => [a] -> [a] -> ([a], [a])
rSwap r1 r2 = (r2, r1)

---- Scales a row: (x)*r
rScale::(Num a) => [a] -> a -> [a]
rScale r x = map (*x) r

---- Adds two rows: r1 + r2
rAdd::(Num a) => [a] -> [a] -> [a]
rAdd [] [] = []
rAdd [] _ = Util.err "Empty row provided as param 1 to rAdd!"
rAdd _ [] = Util.err "Empty row provided as param 2 to rAdd!"
rAdd (h1:t1) (h2:t2) = (h1 + h2) : rAdd t1 t2

---- Subtracts two rows: r1 + (-1)*r2
rSub::(Num a) => [a] -> [a] -> [a]
rSub r1 r2 = rAdd r1 (rScale r2 (-1))


--- **Matrix Functions**

---- Scales a matrix: (x)*m
mScale::(Num a) => (Util.ListMatrix a) -> a -> (Util.ListMatrix a)
mScale m x = map (`rScale` x) m

---- Adds two matrices: m1 + m2
mAdd::(Num a, Ord a) => (Util.ListMatrix a) -> (Util.ListMatrix a) -> (Util.ListMatrix a)
mAdd [] _ = Util.err "Empty matrix provided as param 1 to mAdd!"
mAdd _ [] = Util.err "Empty matrix provided as param 2 to mAdd!"
mAdd m1@(h1:t1) m2@(h2:t2)
    | not sameSize = Util.err "Matrices provided to mAdd have different dimensions!"
    | jagged = Util.err "Jagged matrix provided as a param to mAdd!"
    | null t1 = [(rAdd h1 h2)]
    | otherwise = ((rAdd h1 h2) : (mAdd t1 t2))
    where mxIdx1 = ((numRows m1) - 1)
          mxIdx2 = ((numRows m2) - 1)
          sameSize = (numRows m1 == numRows m2) && (numCols m1 == numCols m2)
          jagged = (checkJagged m1 mxIdx1) || (checkJagged m2 mxIdx2)
          notNull::[a] -> Bool
          notNull x = (not (null x))

---- Subtacts two matrices: m1 + (-1)*m2
mSub::(Num a, Ord a) => (Util.ListMatrix a) -> (Util.ListMatrix a) -> (Util.ListMatrix a)
mSub m1 m2 = mAdd m1 (mScale m2 (-1))

---- Transposes a matrix: swaps the rows with the columns
transpose::(Num a) => (Util.ListMatrix a) -> (Util.ListMatrix a)
transpose m = LST.transpose m

---- Computes the trace of matrix m: sum of the diagonal of m
trace::(Num a, Show a, Ord a) => (Util.ListMatrix a) -> Int -> a
trace m mxIdx
    | not (isSquare m) = Util.err ("Jagged or Non-Square Matrix provided:" ++ show m)
    | mxIdx < 0 = 0
    | otherwise = ((m !! mxIdx) !! mxIdx) + trace m (mxIdx - 1)

---- Computes the determinant of a matrix
determinant::(Num a, Show a, Ord a) => (Util.ListMatrix a) -> a
determinant m
    | not (isSquare m) = Util.err ("Jagged or Non-Square Matrix provided:" ++ show m)
    | rows == 1 = (m !! 0 !! 0)
    | rows == 2 = ((m !! 0 !! 0) * (m !! 1 !! 1) - (m !! 1 !! 0) * (m !! 0 !! 1))
    | rows > 2 = Util.nyi "Determinant for 3x3 and larger matrices not available yet" --TODO det(>2X>2)
    where rows = numRows m
          cols = numCols m


--- **Misc Functions**

---- Returns number of rows
numRows::(Num a) => (Util.ListMatrix a) -> Int
numRows m = length m

---- Returns number of columns in the first row
numCols::(Num a) => (Util.ListMatrix a) -> Int
numCols m = length (m !! 0)

---- Checks if matrix m is jagged. in other words, if matrix m
----   has a varying number of columns per row(thus m is invalid)
----   True if jagged; False otherwise
checkJagged::(Num a, Ord a) => (Util.ListMatrix a) -> Int -> Bool
checkJagged m n
    | len == 0 = True
    | n < 0 = Util.err ("Negative index provided as param 2 to checkJagged: " ++ (show n))
    | n == 0 = ((length (m !! 0)) /= len)
    | otherwise = ((length (m !! n)) /= len) || (checkJagged m (n-1))
    where len = length (m !! 0)

---- Checks if matrix m is a sqaure matrix(# of rows == # of columns)
isSquare::(Num a, Ord a) => (Util.ListMatrix a) -> Bool
isSquare m = (numRows m) == (numCols m) && not (checkJagged m ((numRows m)-1))


--- **Helper Functions**

---- Returns the largest value of an array
maxArr'::(Num a, Ord a) => [a] -> a
maxArr' (x:[]) = x;
maxArr' (h:tl)= max' h (maxArr' (tl))

---- Returns the bigger of two values
max'::(Num a, Ord a) => a -> a -> a
max' x y
     | x > y = x
     | otherwise = y