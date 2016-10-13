-- ***Matrix & Related DataTypes***
module Matrix.Matrix
( 
{- DataTypes -}
  Vector (EmptyVec, ZeroVec)
, Matrix (EmptyMat, ZeroMat)
{- Misc Functions -}
, numComponents
) where

import qualified Matrix.Util as Util

--- **DataType Declearations**

---- Vector:
----   EmptyVec -- 0-D Vector for intenral use
----   ZeroVec  -- N-D Vector of zeros
----   Vec1D    -- 1-D Vector of a Double
----   Vec2D    -- 2-D Vector of Doubles
----   Vec3D    -- 3-D Vector of Doubles
----   VecND    -- N-D Vector of Doubles
data Vector = EmptyVec
              | ZeroVec { v :: Vector }
              | Vec1D { v1 :: Double }
              | Vec2D { v1 :: Double
                      , v2 :: Double
                      }
              | Vec3D { v1 :: Double
                      , v2 :: Double
                      , v3 :: Double
                      }
              | VecND { h :: Double
                      , t :: Vector
                      }
                      deriving (Eq, Ord, Show, Read)

---- Matrix:
----   EmptyMat -- 0x0 Matrix for intenral use
----   IdenMat  -- NxN Identity Matrix
----   ZeroMat  -- NxN Matrix of zeros
----   Mat1x1   -- 1x1 Matrix of a Double
----   Mat2x2   -- 2x2 Matrix of Doubles
----   Mat3x3   -- 3x3 Matrix of Doubles
----   Mat1xN   -- 1x1 Matrix of Doubles
----   Mat2XN   -- 2xN Matrix of Doubles
----   Mat3XN   -- 3xN Matrix of Doubles
----   MatNXN   -- NxN Matrix of Doubles
data Matrix = EmptyMat
            | ZeroMat { mat :: [Vector] }
            | IdenMat { matIden :: [Vector] }
            | Mat1x1  { r1 :: Vector }
            | Mat2x2  { r1 :: Vector
                      , r2 :: Vector
                      }
            | Mat3x3  { r1 :: Vector
                      , r2 :: Vector
                      , r3 :: Vector
                      }
            | Mat1xN  { r1 :: Vector }
            | Mat2xN  { r1 :: Vector
                      , r2 :: Vector
                      }
            | Mat3xN  { r1 :: Vector
                      , r2 :: Vector
                      , r3 :: Vector
                      }
            | MatNxM { mat :: [Vector] }
                     deriving (Eq, Ord, Show, Read)

--- **DataType Creation Functions**

--- *Vector*

---- Returns an empty Vector
makeEmptyVec:: Vector
makeEmptyVec = EmptyVec

---- Returns a ZeroVec of N dimension
makeZeroVec:: Int -> Vector
makeZeroVec n = (ZeroVec { v=(makeVecNDInternal n)})
          where makeVecNDInternal:: Int -> Vector
                makeVecNDInternal n
                                | n <= 1 = EmptyVec
                                | otherwise = (VecND { h=0, t=(getValOrErr (makeVecND (replicate (n-1) 0) (n-1))) })

---- Returns a Vec1D
makeVec1D:: Double -> Vector
makeVec1D x = (Vec1D { v1=x })

---- Returns a Vec2D
makeVec2D:: (Double, Double) -> Vector
makeVec2D (x, y) = (Vec2D { v1=x, v2=y })

---- Returns a Vec3D
makeVec3D:: (Double, Double, Double) -> Vector
makeVec3D (x, y, z) = (Vec3D { v1=x, v2=y, v3=z })

---- Returns a VecND
makeVecND:: [Double] -> Int -> Either String Vector
makeVecND l@(vi:v) n
                | null l || n <= 0 = Left $ "Vectors must have dimension >= 1"
                | null v || n == 1 = Right (VecND { h=vi, t=(makeEmptyVec) })
                | otherwise = Right (VecND { h=vi, t=(getValOrErr (makeVecND v (n-1))) })


--- *Matrix*

---- Returns an empty Matrix
makeEmptyMat:: Matrix
makeEmptyMat = EmptyMat

---- Returns a ZeroMat of RxC dimension
makeZeroMat:: Int -> Int -> Either String Matrix
makeZeroMat 0 _ = Left $ "A matrix must have at least 1 row"
makeZeroMat _ 0 = Left $ "A matrix must have at least 1 column"
makeZeroMat r c = Right (ZeroMat { mat=(replicate r (makeZeroVec c)) })

---- Returns a Mat1x1
makeMat1x1:: Vector -> Either String Matrix
makeMat1x1 v1@(Vec1D _) = Right (Mat1x1 { r1=v1 })
makeMat1x1 v1 = Left $ "Vector " ++ (show v1) ++ " does not have the same dimension as a row for a: " ++ "1x1 Matrix"

---- Returns a Mat2x2
makeMat2x2:: (Vector, Vector) -> Either String Matrix
makeMat2x2 (v1@(Vec2D _ _), v2@(Vec2D _ _)) = Right (Mat2x2 { r1=v1, r2=v2 })
makeMat2x2 (v1, v2) = Left $ "Vectors " ++ (show v1) ++ " and " ++ (show v2) ++ " do not have the same dimension as a row for a: " ++ "2x2 Matrix"

---- Returns a Mat3x3
makeMat3x3:: (Vector, Vector, Vector) -> Either String Matrix
makeMat3x3 (v1@(Vec3D _ _ _), v2@(Vec3D _ _ _), v3@(Vec3D _ _ _)) = Right (Mat3x3 { r1=v1, r2=v2, r3=v3 })
makeMat3x3 (v1, v2, v3) = Left $ "Vectors " ++ (show v1) ++ ", " ++ (show v2) ++ " and " ++ (show v3) ++ " do not have the same dimension as a row for a: " ++ "3x3 Matrix"

---- Returns a Mat1xN
makeMat1xN:: Int -> Vector -> Either String Matrix
makeMat1xN cols v1@(VecND _ _) 
            | (numComponents v1) == cols = Right (Mat1xN { r1=v1 })
            | otherwise = Left $ "Vector " ++ (show v1) ++ "does not have " ++ (show cols) ++ " components, required for a:" ++ "1xN Matrix, with N=" ++ (show cols)
makeMat1xN cols v1 = Left $ "Vector " ++ (show v1) ++ " is not of type VecND, as required for a: " ++ "1xN Matrix, with N=" ++ (show cols)

---- Returns a Mat2xN
makeMat2xN:: Int -> (Vector, Vector) -> Either String Matrix
makeMat2xN cols (v1@(VecND _ _), v2@(VecND _ _))
            | ((numComponents v1) == (numComponents v2)) && ((numComponents v1) == cols) = Right (Mat2xN { r1=v1, r2=v2 })
            | otherwise = Left $ "Vectors " ++ (show v1) ++ " and " ++ (show v2) ++ "do not have " ++ (show cols) ++ " components, required for a:" ++ "2xN Matrix, with N=" ++ (show cols)
makeMat2xN cols (v1,  v2) = Left $ "Vectors " ++ (show v1) ++ " and " ++ (show v2) ++ " are not of type VecND, as required for a: " ++ "2xN Matrix, with N=" ++ (show cols)

---- Returns a Mat3xN
makeMat3xN:: Int -> (Vector, Vector, Vector) -> Either String Matrix
makeMat3xN cols (v1@(VecND _ _), v2@(VecND _ _), v3@(VecND _ _))
            | ((numComponents v1) == (numComponents v2)) && ((numComponents v2) == (numComponents v3)) && ((numComponents v1) == cols) = Right (Mat3xN { r1=v1, r2=v2, r3=v3 })
            | otherwise = Left $ "Vectors " ++ (show v1) ++ ", " ++ (show v2) ++ " and " ++ (show v3) ++ "do not have " ++ (show cols) ++ " components, required for a:" ++ "3xN Matrix, with N=" ++ (show cols)
makeMat3xN cols (v1, v2, v3) = Left $ "Vectors " ++ (show v1) ++ ", " ++ (show v2) ++ " and " ++ (show v3) ++ " are not of type VecND, as required for a: " ++ "3xN Matrix, with N=" ++ (show cols)

---- Returns a MatNxM of RxC dimension
makeMatNxM:: Int -> Int -> [[Double]] -> Either String (Either Vector Matrix)
makeMatNxM 0 _ _ = Left $ "A matrix must have at least 1 row"
makeMatNxM _ 0 _ = Left $ "A matrix must have at least 1 column"
makeMatNxM r c [] = Right $ Right (MatNxM { mat=(replicate r (getValOrErr (makeVecND (replicate c 0) (c-1)))) })
makeMatNxM r c (h:t)
                | null t = Right $ Left (getValOrErr (makeVecND h c))
                | otherwise = Right $ Right (MatNxM { mat=((getValOrErr (makeVecND h c)) : [getLeft (getRightLROrErr (makeMatNxM (r-1) c t))]) })
                where getRightLROrErr:: Either String (Either Vector Matrix) -> (Either Vector Matrix)
                      getRightLROrErr (Right (Left rl)) = Left rl
                      getRightLROrErr (Right (Right rr)) = Right rr
                      getRightLROrErr (Left l)= Util.err l
                      getLeft:: Either Vector Matrix -> Vector
                      getLeft (Left l) = l
                      getRight:: Either Vector Matrix -> Matrix
                      getRight (Right r) = r


--- **Misc Functions**

---- Returns the number of components for a given vector
numComponents:: Vector -> Int
numComponents (EmptyVec) = 0
numComponents v@(ZeroVec _) = (numComponents (v))
numComponents (Vec1D _) = 1
numComponents (Vec2D _ _) = 2
numComponents (Vec3D _ _ _) = 3
numComponents (VecND _ t) = 1 + (numComponents t)

getValOrErr:: Either String Vector -> Vector
getValOrErr (Right b) = b
getValOrErr (Left a) = Util.err a