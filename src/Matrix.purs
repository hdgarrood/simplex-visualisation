module Matrix
  ( Matrix
  , unsafeMatrix
  , toColumnArray
  , matrixMultiply, (.*)
  , matrixAdd, (.+)
  , dot
  , transpose
  , prettyPrint
  ) where

import Prelude
import Data.Foldable (sum, maximum)
import Data.String as String
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Typelevel.Num (class Nat, toInt)
import Data.Typelevel.Undefined (undefined)
-- import Type.Equality (class TypeEquals)
import Data.Enum (enumFromTo)
import Data.Array as Array
import Partial.Unsafe (unsafePartial)

-- | A Matrix with m rows and n columns. Stored as an array of columns.
newtype Matrix m n a = Matrix (Array (Array a))

derive newtype instance eqMatrix :: Eq a => Eq (Matrix m n a)

instance functorMatrix :: Functor (Matrix m n) where
  map f (Matrix xss) = Matrix (map (map f) xss)

-- | Construct a matrix without dimensions checking.
unsafeMatrix :: forall m n a. Array (Array a) -> Matrix m n a
unsafeMatrix = Matrix

-- | O(1). Convert a matrix to an array of columns.
toColumnArray :: forall m n a. Matrix m n a -> Array (Array a)
toColumnArray (Matrix xss) = xss

-- | something like O(n). Convert a matrix to an array of rows.
toRowArray :: forall m n a. Matrix m n a -> Array (Array a)
toRowArray = toColumnArray <<< transpose

dot :: forall a. Semiring a => Array a -> Array a -> a
dot x y = sum (Array.zipWith (*) x y)

transposeArray :: forall a. Array (Array a) -> Array (Array a)
transposeArray xss =
  case Array.head xss of
    Nothing ->
      []
    Just xs ->
      Array.mapWithIndex
        (\i x -> map (\ys -> unsafePartial (Array.unsafeIndex ys i)) xss)
        xs

transpose :: forall m n a. Matrix m n a -> Matrix n m a
transpose (Matrix xss) = Matrix (transposeArray xss)

matrixAdd :: forall m n a.
  Semiring a =>
  Matrix m n a ->
  Matrix m n a ->
  Matrix m n a
matrixAdd (Matrix a) (Matrix b) =
  Matrix (Array.zipWith (Array.zipWith (+)) a b)

infixl 6 matrixAdd as .+

matrixMultiply :: forall m n k a.
  Semiring a =>
  Matrix m n a ->
  Matrix n k a ->
  Matrix m k a
matrixMultiply a b =
  let
    aRows = toRowArray a
    bCols = toColumnArray b
  in
    Matrix (map (\col -> map (dot col) aRows) bCols)

infixl 7 matrixMultiply as .*

identityM :: forall n a. Nat n => Semiring a => Matrix n n a
identityM = Matrix cols
  where
  size = toInt (undefined :: n)
  cols = map makeCol (enumFromTo 1 size)
  makeCol i =
    Array.replicate (i - 1) zero
    <> [one]
    <> Array.replicate (size - i) zero

zeroM :: forall m n a. Nat m => Nat n => Semiring a => Matrix m n a
zeroM = Matrix (Array.replicate ncols (Array.replicate nrows zero))
  where            
  nrows = toInt (undefined :: m)
  ncols = toInt (undefined :: n)

instance semiringMatrix :: (Nat n, Semiring a) => Semiring (Matrix n n a) where
  zero = zeroM
  add = matrixAdd
  one = identityM
  mul = matrixMultiply

instance ringMatrix :: (Nat n, Ring a) => Ring (Matrix n n a) where
  sub a b = add a (map negate b)

prettyPrint :: forall m n a. Show a => Matrix m n a -> String
prettyPrint m =
  let
    cols = toColumnArray m

    formatCol :: Array a -> Array String
    formatCol col =
      let
        shown =
          map show col
        width =
          fromMaybe 0 (maximum (map String.length shown))
      in
        map (leftPad width) shown

    joinFormattedCols :: Array (Array String) -> String
    joinFormattedCols =
      transposeArray
      >>> map (String.joinWith " ")
      >>> String.joinWith "\n"
  in
    joinFormattedCols (map formatCol cols)

leftPad :: Int -> String -> String
leftPad width str =
  let
    len = String.length str
  in
   if len >= width
     then str
     else String.fromCharArray (Array.replicate (width - len) ' ') <> str

instance showMatrix :: Show a => Show (Matrix m n a) where
  show = prettyPrint
