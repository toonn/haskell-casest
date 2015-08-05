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

natter :: Natty n -> Nat
natter Zy = Z
natter (Sy n) = S (natter n)


data Fin :: Nat -> * where
  Zf :: Fin (S n)
  Sf :: Fin n -> Fin (S n)

data Vec :: * -> Nat -> * where
  V0   :: Vec a Z
  (:>) :: a -> Vec a n -> Vec a (S n)
infixr 5 :>

vlookup :: Fin n -> Vec a n -> a
vlookup (Zf)   (a :> _)  = a
vlookup (Sf n) (_ :> as) = vlookup n as

vreplicate :: Natty n -> a -> Vec a n
vreplicate Zy     _ = V0
vreplicate (Sy n) a = a :> vreplicate n a

vreverse :: Vec a n -> Vec a n
vreverse V0 = V0
vreverse (a :> V0) = a :> V0
vreverse (a :> as) = vreverse' as a
  where
    vreverse' :: Vec a n -> a -> Vec a (S n)
    vreverse' V0 x' = x' :> V0
    vreverse' (x :> xs) x' = x :> vreverse' xs x'
    

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

data Position = Pos { getX      :: Nat
                    , getY      :: Nat
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
exPath = Pcons (Posy Zy Zy Gasy Lowy) Next
        (Pcons (Posy (Sy Zy) Zy Gasy Lowy) Back
        (Pcons (Posy Zy Zy Gasy Lowy) Stay P0))

matterToPosVec :: Vec Material n -> Vec Material n -> Nat -> Nat
                   -> Vec Position n
matterToPosVec V0 V0 _ _ = V0
matterToPosVec (mat :> mats) (under :> unders) x y =
  Pos x y mat cl :> matterToPosVec mats unders (S x) y
    where
      clearance :: Material -> Material -> Clearance
      clearance Gas   Gas   = High
      clearance Gas   Solid = Low
      clearance Solid _     = God
      cl = clearance mat under

matterToPosVecs :: Natty w -> Natty h -> Vec (Vec Material w) h
                    -> Vec (Vec Position w) h
matterToPosVecs _ _ V0 = V0
matterToPosVecs w (Sy z) (mats :> matss) =
  matterToPosVec mats (unders w matss Gas) Z y  :> matterToPosVecs w z matss
    where
      y = natter (Sy z)
      unders :: Natty m -> Vec (Vec a m) n -> a -> Vec a m
      unders m V0 fallback = vreplicate m fallback
      unders _ (us :> _) _ = us

mattersToMatrix :: Natty w -> Natty h -> Vec (Vec Material w) h
                    -> Matrix Position w h
mattersToMatrix w h matss = Mat (vreverse (matterToPosVecs w h matss))

o :: Material
o = Gas
c :: Material
c = Solid

exampleLevel :: Matrix Position
                        (S (S (S (S (S (S (S (S (S (S Z))))))))))
                        (S (S (S (S (S (S (S Z)))))))
exampleLevel = mattersToMatrix
                  (Sy (Sy (Sy (Sy (Sy (Sy (Sy (Sy (Sy (Sy Zy))))))))))
                  (Sy (Sy (Sy (Sy (Sy (Sy (Sy Zy)))))))
                  (
  (o :> o :> o :> o :> o :> o :> o :> o :> o :> o :> V0) :>
  (o :> o :> o :> o :> o :> o :> c :> c :> c :> o :> V0) :>
  (o :> o :> o :> o :> o :> o :> o :> o :> o :> o :> V0) :>
  (o :> o :> o :> c :> c :> c :> o :> o :> o :> o :> V0) :>
  (o :> o :> o :> o :> o :> o :> o :> o :> o :> o :> V0) :>
  (c :> o :> o :> o :> o :> o :> o :> o :> o :> c :> V0) :>
  (c :> c :> c :> c :> c :> o :> o :> c :> c :> c :> V0) :> V0)

