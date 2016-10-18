-- ***ListMatrix Operators***
module MathUtil.Operators
(
{- Row Operators -}
  (<=>)
, (=*)
, (=+)
, (=-)
{- Matrix Operators -}
, (#*)
, (#+)
, (#-)
, (--|-)
, tr
, det
) where


--- **Imports**
import qualified MathUtil.Operations as Ops
import qualified MathUtil.Util as Util


--- **Row Operators**

---- Swaps two rows and puts them in a tuple
(<=>)::(Num a) => [a] -> [a] -> ([a], [a])
r1 <=> r2 = Ops.rSwap r1 r2

---- Scales a row: (x)*r
(=*)::(Num a) => a -> [a] -> [a]
x =* r = Ops.rScale r x

---- Adds two rows: r1 + r2
(=+)::(Num a) => [a] -> [a] -> [a]
r1 =+ r2 = Ops.rAdd r1 r2

---- Subtracts two rows: r1 + (-1)*r2
(=-)::(Num a) => [a] -> [a] -> [a]
r1 =- r2 = Ops.rSub r1 r2


--- **Matrix Operators**

---- Scales a matrix: (x)*m
(#*)::(Num a) => a -> (Util.ListMatrix a) -> (Util.ListMatrix a)
x #* r = Ops.mScale r x

---- Adds two matrices: m1 + m2
(#+)::(Num a, Ord a) => (Util.ListMatrix a) -> (Util.ListMatrix a) -> (Util.ListMatrix a)
m1 #+ m2 = Ops.mAdd m1 m2

---- Subtacts two matrices: m1 + (-1)*m2
(#-)::(Num a, Ord a) => (Util.ListMatrix a) -> (Util.ListMatrix a) -> (Util.ListMatrix a)
m1 #- m2 = Ops.mSub m1 m2

---- Transpose matrix m: m^T or "m dagger"
(--|-)::(Num a) => (Util.ListMatrix a) -> (Util.ListMatrix a)
(--|-) m = Ops.transpose m

---- Performs trace on matrix m
tr::(Num a, Show a, Ord a) => (Util.ListMatrix a) -> Int -> a
tr m mxIndx = Ops.trace m mxIndx

---- Computes the determinant of matrix m
det::(Num a, Show a, Ord a) => (Util.ListMatrix a) -> a
det m = Ops.determinant m
