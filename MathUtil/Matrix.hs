-- ***Matrix***
module MathUtil.Matrix
( 
{- DataTypes -}
  Matrix ()
{- Factory Functions -}
, makeMatrixFiller
, makeMatrixLinkedList
, makeIdentityMatrix
, makeZeroMatrix
, listToMatrix
, matrixToList
{- Methods -}
, scale
, mAdd
, mSub
, transpose
, trace
, determinant
, coFactor
, numRows
, numCols
, size
, isSquare
, scaleRow
, sumRow
{- Misc Functions -}
, getMaybeRow
, getRightMatrix
) where


--- **Imports**
import qualified Data.List as LST -- for transpose

import qualified MathUtil.Util as Util
import MathUtil.LinkedList


--- **Type Synomyms**

---- Treat LinkedLists or Doubles as Rows
type Row = (LinkedList Double)

---- Treat (Int,Int) pairs are Dimensions for Matricies
type Dim = (Int, Int)


--- **DataType Declarations**

---- Matrix: a Matrix of Doubles
data Matrix = Matrix (LinkedList Row) deriving (Eq, Ord, Show, Read)


--- **DataType Factory Functions**

---- Creates a Matrix, with param 1 as (rows, cols), and param 2 as the default value
----   Returns an error String instead if either rows or cols is <= 0
makeMatrixFiller:: (Int, Int) -> Double -> Either String Matrix
makeMatrixFiller (0,_) _ = Left $ getErrMsg 10 "makeMatrixFiller"
makeMatrixFiller (_,0) _ = Left $ getErrMsg 10 "makeMatrixFiller"
makeMatrixFiller dim val = Right (Matrix (makeMatrixFillerInternal dim val))
            where makeMatrixFillerInternal:: (Int, Int) -> Double -> (LinkedList Row)
                  makeMatrixFillerInternal (0,_) _ = EmptyList
                  makeMatrixFillerInternal (_,0) _ = EmptyList
                  makeMatrixFillerInternal (r,c) val = Node (makeLinkedListFiller c val) (makeMatrixFillerInternal (r-1,c) val)

---- Converts a LinkedList of LinkedLists into a Matrix
----   Returns an error String instead if param 1 is an EmptyList
makeMatrixLinkedList:: (LinkedList Row) -> Either String Matrix
makeMatrixLinkedList (EmptyList) = Left $ getErrMsg 10 "makeMatrixLinkedList"
makeMatrixLinkedList llist = if checkJagged mat then Left (getErrMsg 11 "makeMatrixLinkedList") else Right mat
            where mat = Matrix llist

---- Constructs the Identity Matrix of dimension (param 1) by (param 1)
----   Returns an error String instead if param 1 is <= 0
makeIdentityMatrix:: Int -> Either String Matrix
makeIdentityMatrix size
            | size <= 0 = Left $ getErrMsg 10 "makeIdentityMatrix"
            | otherwise = (listToMatrix (makeIdentityList size (size-1)))
            where makeIdentityList:: Int -> Int -> [[Double]]
                  makeIdentityList size row
                            | size <= 0 || row < 0 = [[]]
                            | row == 0 =  [(fromIntegral 1) : [ (fromIntegral (x-x)) | x <- [0..(size-2)] ]]
                            | otherwise = (makeIdentityList size (row-1)) ++ [[ y | x <- [0..(size-1)], let y = (if x /= row then (fromIntegral 0) else (fromIntegral 1)) ]]

---- Shortcut for: makeMatrixFiller (param 1) 0
----   See makeMatrixFiller
makeZeroMatrix:: (Int,Int) -> Either String Matrix
makeZeroMatrix dim = (makeMatrixFiller dim 0)

---- Converts a List of Lists of Doubles into a Matrix
----   Returns an error String instead if param 1 is [] or [[]]
listToMatrix:: [[Double]] -> Either String Matrix
listToMatrix [] = Left $ getErrMsg 10 "listToMatrix"
listToMatrix [[]] = Left $ getErrMsg 10 "listToMatrix"
listToMatrix list = if checkJagged mat then Left (getErrMsg 11 "listToMatrix") else Right mat
            where mat = Matrix (listToMatrixInternal list)
                  listToMatrixInternal:: [[Double]] -> (LinkedList Row)
                  listToMatrixInternal [] = EmptyList
                  listToMatrixInternal (h:t) = Node (listToLinkedList h) (listToMatrixInternal t)

---- Converts a matrix to a list of lists
matrixToList:: Matrix -> [[Double]]
matrixToList (Matrix  m) = matrixToListInternal m
            where matrixToListInternal:: (LinkedList Row) -> [[Double]]
                  matrixToListInternal (EmptyList) = []
                  matrixToListInternal m@(Node d t) = (linkedListToList d) : (matrixToListInternal t)


--- **DataType Methods**

---- Scales a Matrix: (x)*m
scale:: Matrix -> Double -> Matrix
scale (Matrix (EmptyList)) _ = Matrix $ EmptyList
scale (Matrix m) x = Matrix $ scaleInternal x m
            where scaleInternal:: Double -> (LinkedList Row) -> (LinkedList Row)
                  scaleInternal x (Node r (EmptyList)) = makeNode (scaleRow r x)
                  scaleInternal x m@(Node r rs) = makeNodeT (scaleRow r x) (scaleInternal x rs)

---- Adds two matrices: m1 + m2
mAdd:: Matrix -> Matrix -> Either String Matrix
mAdd mat1@(Matrix m1) mat2@(Matrix m2)
    | not sameSize = Left $ getErrMsg 20 "mAdd"
    | otherwise = Right $ Matrix (mAddInternal m1 m2)
    where sameSize = (size mat1) == (size mat2)
          mAddInternal:: (LinkedList Row) -> (LinkedList Row) -> (LinkedList Row)
          mAddInternal (EmptyList) m = m
          mAddInternal m (EmptyList) = m
          mAddInternal m1@(Node d1 t1) m2@(Node d2 t2) = (makeNodeT (sumRow d1 d2) (mAddInternal t1 t2))

---- Subtacts two matrices: m1 + (-1)*m2
mSub:: Matrix -> Matrix -> Either String Matrix
mSub m1 m2 = mAdd m1 (scale m2 (-1))

---- Transposes a matrix: swaps the rows with the columns
transpose:: Matrix -> Matrix
transpose m = transposeInternal (listToMatrix (LST.transpose (matrixToList m)))
            where transposeInternal:: Either String Matrix -> Matrix
                  transposeInternal (Left s) = Util.err s
                  transposeInternal (Right mat) = mat

---- Computes the trace of matrix m: sum of the diagonal of m
trace:: Matrix -> Either String Double
trace mat@(Matrix m)
    | not (isSquare mat) = Left $ getErrMsg 21 "trace"
    | otherwise = Right (computeTrace m (0,(snd sizeM)-1))
    where sizeM = size mat
          computeTrace:: (LinkedList Row) -> (Int,Int) -> Double
          computeTrace (EmptyList) _ = 1.0
          computeTrace m@(Node r rs) (cnt,cols)
                    | cnt > cols = 1.0
                    | otherwise = (getMaybeDouble (getData (getNodeRec r (cnt))) 1.0) * (computeTrace rs (cnt+1,cols))

---- Computes the determinant of a Matrix <= 2x2, or the First Row Expansion of a Matrix > 2x2
determinant:: Matrix -> Either String Double
determinant (Matrix (EmptyList)) = Left $ getErrMsg 22 "determinant"
determinant mat@(Matrix m@(Node r@(Node d t) rs))
    | not (isSquare mat) = Left $ getErrMsg 21 "determinant"
    | (fst sizeM) == 1 = Right $ (getMaybeDouble (getData (getMaybeRow (getData m))) 1.0)
    | (fst sizeM) == 2 = Right $ ((getMaybeDouble (getData (getNodeRec r 0)) 1.0) * (getMaybeDouble (getData (getNodeRec (getMaybeRow (getData rs)) 1)) 1.0)) - ((getMaybeDouble (getData (getNodeRec (getMaybeRow (getData rs)) 0)) 1.0) * (getMaybeDouble (getData (getNodeRec r 1)) 1.0))
    | otherwise = Right $ sum [ e | n <- [0..((snd sizeM)-1)], let e = ((getMaybeDouble (getData (getNodeRec r n)) 1.0) * (coFactor mat (0,n))) ]
    where sizeM = size mat

---- Returns the `m,n coFactor' of param 1, with param 2 providing (m,n)
coFactor:: Matrix -> (Int,Int) -> Double
coFactor (Matrix (EmptyList)) _ = 0
coFactor mat@(Matrix m@(Node r rs)) (row,col) = (sgn (row+col)) * (getRightDouble minor)
            where sizeM = size mat
                  minor = determinant (getRightMatrix (listToMatrix (LST.transpose igColList)))
                  igRowList = [ (fst rowN) | rowN <- (zip (matrixToList mat) [0..((fst sizeM)-1)]), (snd rowN) /= row ]
                  igColList = [ (fst rowN) | rowN <- (zip (LST.transpose igRowList) [0..((snd sizeM)-1)]), (snd rowN /= col) ]
                  sgn:: Int -> Double
                  sgn x
                    | odd x = -1.0
                    | even x = 1.0

---- Returns number of rows
numRows:: Matrix -> Int
numRows (Matrix m) = len m

---- Returns number of columns in the first row
numCols:: Matrix -> Int
numCols (Matrix (Node d _)) = len d

---- Gets the dimensions of a matrix
size:: Matrix -> Dim
size m = (numRows m, numCols m)

---- Checks if matrix m is jagged. in other words, if matrix m
----   has a varying number of columns per row(thus m is invalid)
----   True if jagged; False otherwise
checkJagged:: Matrix -> Bool
checkJagged (Matrix m) = checkJaggedInternal m
    where leng = len $ getMaybeRow (getData m)
          checkJaggedInternal:: (LinkedList Row) -> Bool
          checkJaggedInternal (EmptyList) = False
          checkJaggedInternal (Node r rs) = ((len r) /= leng) || (checkJaggedInternal rs)

---- Checks if matrix m is a sqaure matrix(# of rows == # of columns)
isSquare:: Matrix -> Bool
isSquare m = (numRows m) == (numCols m)


--- **Helper Functions**

---- Scales the data in param 1 by param 2
scaleRow:: Row -> Double -> Row
scaleRow (EmptyList) _ = EmptyList
scaleRow (Node d t) x = makeNodeT (d * x) (scaleRow t x)

---- Sums the data in param 1 with the corresponding data in param 2
sumRow::Row -> Row -> Row
sumRow (EmptyList) (EmptyList) = EmptyList
sumRow r (EmptyList) = r
sumRow (EmptyList) r = r
sumRow (Node d1 t1) (Node d2 t2) = makeNodeT (d1+d2) (sumRow t1 t2)


--- **Misc Functions**

---- Gets a Row from a `Maybe Row'
getMaybeRow:: (Maybe Row) -> Row
getMaybeRow mr = Util.getMaybeVal mr EmptyList

---- Gets a Double from a `Maybe Double'
getMaybeDouble:: (Maybe Double) -> Double -> Double
getMaybeDouble md x = Util.getMaybeVal md x

---- Gets a Matrix from a `Either String Matrix'
getRightMatrix:: Either String Matrix -> Matrix
getRightMatrix esm = Util.getRightVal esm

---- Gets a Double from a `Either String Double'
getRightDouble:: Either String Double -> Double
getRightDouble esd = Util.getRightVal esd

---- Returns an error message from given a error code
getErrMsg:: Int -> String -> String
getErrMsg 10 context = (getErrPrefix context) ++ "Matrix must be of size 1x1 or greater!"
getErrMsg 11 context = (getErrPrefix context) ++ "Jagged Matrices are invalid!"
getErrMsg 20 context = (getErrPrefix context) ++ "Cannot Add/Substract Matrices of different dimensions!"
getErrMsg 21 context = (getErrPrefix context) ++ "Non-Square Matrix Provided"
getErrMsg 22 context = (getErrPrefix context) ++ "Empty Matrix Provided"

getErrPrefix:: String -> String
getErrPrefix context = "[Matrix/Matrix.hs: " ++ context ++ "]"