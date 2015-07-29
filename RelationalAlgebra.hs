{-
 
    Practical Relational Algebra
            Toon Nolten
              base on
          The Power of Pi
 
-}
{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeFamilies #-}

module RelationalAlgebra where

parens :: String -> String
parens str = "(" ++ str ++ ")"

