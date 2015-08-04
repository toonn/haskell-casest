{-

    Verified Koopa Troopa Movement
             Toon Nolten

-}

{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Koopa where

data Nat = Z | S Nat
data Natty :: Nat -> * where
  Zy :: Natty Z
  Sy :: Natty n -> Natty (S n)

data Fin :: Nat -> * where
  Zf :: Fin (S n)
  Sf :: Fin n -> Fin (S n)

data Vec :: * -> Nat -> * where
  V0   :: Vec a Z
  (:>) :: a -> Vec a n -> Vec a (S n)

vlookup :: Fin n -> Vec a n -> a
vlookup (Zf) (a :> _) = a
vlookup (Sf n) (_ :> as) = vlookup n as

data Matrix :: * -> Nat -> Nat -> * where
  Mat :: Vec (Vec a w) h -> Matrix a w h

lookup :: Fin h -> Fin w -> Matrix a w h -> a
lookup row column (Mat rows) = vlookup column (vlookup row rows)

data Color = Green | Red
data Colorry :: Color -> * where
  Greeny :: Colorry Green
  Redy   :: Colorry Red

data KoopaTroopa :: Color -> * where
  KT :: Colorry c -> KoopaTroopa c

data Material = Gas | Solid
data Matty :: Material -> * where
  Gasy   :: Matty Gas
  Solidy :: Matty Solid

data Clearance = Low | High | God
data Clearry :: Clearance -> * where
  Lowy  :: Clearry Low
  Highy :: Clearry High
  Gody  :: Clearry God

data Position = Pos { x      :: Nat
                    , y      :: Nat
                    , matter :: Material
                    , clr    :: Clearance
                    }
data Positionny :: Position -> * where
  Posy :: Natty x -> Natty y -> Matty m -> Clearry cl
            -> Positionny (Pos x y m cl)

-- data CoClr :: Color -> Clearance -> * where
--   RedClr   :: CoClr c Low
--   GreenClr :: CoClr Green High

class CoClr (c :: Color) (cl :: Clearance)
instance CoClr c Low
instance CoClr Green High

data Follows :: Position -> Position -> Color -> * where
  Stay :: Follows (pos x y Gas Low) (pos x y Gas Low) c
  Next :: CoClr c cl => Follows (pos (S x) y Gas cl) (pos x y Gas Low) c
  Back :: CoClr c cl => Follows (pos x y Gas cl) (pos (S x) y Gas Low) c
  Fall :: Follows (pos x y Gas cl) (pos x (S y) Gas High) c

data Path :: Color -> Position -> Position -> * where
  P0 :: Path c p p
  Pcons :: Positionny p -> Follows q p c -> Path c q r -> Path c p r


-- Examples

exPath :: Path Red (Pos Z Z Gas Low) (Pos Z Z Gas Low)
exPath = Pcons (Posy Zy Zy Gasy Lowy)
                Next
                (Pcons (Posy (Sy Zy) Zy Gasy Lowy)
                       Back
                       (Pcons (Posy Zy Zy Gasy Lowy)
                              Stay
                              P0))
