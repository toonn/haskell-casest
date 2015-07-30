{-
    Universe of types available in queries
                 Toon Nolten

    No nested modules in Haskell makes for many modules
-}

{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeFamilies #-}

module Universe where

data Nat = Z | S Nat deriving (Show, Eq, Ord)

data Vec :: * -> Nat -> * where
  V0   :: Vec a Z
  (:>) :: a -> Vec a n -> Vec a (S n)

data U = CHAR | NAT | BOOL | VEC U Nat deriving (Show, Eq, Ord)

type family El (u :: U)
type instance El CHAR      = Char
type instance El NAT       = Nat
type instance El BOOL      = Bool
type instance El (VEC u n) = Vec (El u) n
