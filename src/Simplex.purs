module Simplex where

import Matrix (Matrix, Vector)

-- | An LP problem with m constraints and n variables (excluding slack
-- | variables), where the objective function should be maximised and the
-- | inequality is less-than-or-equal-to, i.e. each entry of Ax should be less
-- | than or equal to the corresponding entry of b.
-- |
-- | The costs vector is often abbreviated as "c", the coefficient matrix as
-- | "A", the vector of constraint bounds as "b", and the vector of decision
-- | variables as "x".
newtype LP m n
 = LP
     { costs :: Vector n Number
     , coefficients :: Matrix m n Number
     , bounds :: Vector m Number
     }


