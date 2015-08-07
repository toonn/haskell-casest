{-

    Verified Koopa Troopa Movement
             Toon Nolten

-}

{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Koopa where

data Some :: (k -> *) -> * where
  Like :: p x -> Some p

data Nat = Z | S Nat
data Natty :: Nat -> * where
  Zy :: Natty Z
  Sy :: Natty n -> Natty (S n)
natter :: Natty n -> Nat
natter Zy = Z
natter (Sy n) = S (natter n)
nattyer :: Nat -> Some Natty
nattyer Z = Like Zy
nattyer (S n) = case nattyer n of Like m -> Like (Sy m)


data Fin :: Nat -> * where
  Zf :: Fin (S n)
  Sf :: Fin n -> Fin (S n)
intToFin :: Natty n -> Integer -> Fin n
intToFin Zy _ = error "Fin Z is an empty type"
intToFin (Sy n) i
  | i <  0 = error "Negative Integers cannot be represented by a finite natural"
  | i == 0 = Zf
  | i >  0 = Sf (intToFin n (i-1))


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

mlookup :: Fin h -> Fin w -> Matrix a w h -> a
mlookup row column (Mat rows) = vlookup column (vlookup row rows)

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
mattyer :: Material -> Some Matty
mattyer Gas = Like Gasy
mattyer Solid = Like Solidy

data Clearance = Low | High | God
data Clearry :: Clearance -> * where
  Lowy  :: Clearry Low
  Highy :: Clearry High
  Gody  :: Clearry God
clearryer :: Clearance -> Some Clearry
clearryer Low = Like Lowy
clearryer High = Like Highy
clearryer God = Like Gody


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
                       (S(S(S(S(S(S(S(S(S(S Z)))))))))) -- 10
                       (S(S(S(S(S(S(S Z))))))) -- 7
exampleLevel = mattersToMatrix
                 (Sy(Sy(Sy(Sy(Sy(Sy(Sy(Sy(Sy(Sy Zy)))))))))) -- 10
                 (Sy(Sy(Sy(Sy(Sy(Sy(Sy Zy))))))) -- 7
                 (
  (o :> o :> o :> o :> o :> o :> o :> o :> o :> o :> V0) :>
  (o :> o :> o :> o :> o :> o :> c :> c :> c :> o :> V0) :>
  (o :> o :> o :> o :> o :> o :> o :> o :> o :> o :> V0) :>
  (o :> o :> o :> c :> c :> c :> o :> o :> o :> o :> V0) :>
  (o :> o :> o :> o :> o :> o :> o :> o :> o :> o :> V0) :>
  (c :> o :> o :> o :> o :> o :> o :> o :> o :> c :> V0) :>
  (c :> c :> c :> c :> c :> o :> o :> c :> c :> c :> V0) :> V0)


ix :: Integer -> Fin (S(S(S(S(S(S(S(S(S(S Z)))))))))) -- 10
ix = intToFin (Sy(Sy(Sy(Sy(Sy(Sy(Sy(Sy(Sy(Sy Zy)))))))))) -- 10

iy :: Integer -> Fin (S(S(S(S(S(S(S Z))))))) -- 7
iy = intToFin (Sy(Sy(Sy(Sy(Sy(Sy(Sy Zy))))))) -- 7

p :: Fin (S(S(S(S(S(S(S(S(S(S Z))))))))))
     -> Fin (S(S(S(S(S(S(S Z)))))))
     -> Some Positionny
p x y | Pos x' y' m' cl' <- mlookup y x exampleLevel
      , Like xy <- nattyer x'
      , Like yy <- nattyer y'
      , Like my <- mattyer m'
      , Like cly <- clearryer cl'
      = Like (Posy xy yy my cly)

redPathOne :: Path Red (Pos (S(S(S(S(S(S(S(S Z))))))))
                            (S(S(S(S(S(S Z)))))) Gas Low)
                       (Pos (S(S(S(S(S(S(S(S Z))))))))
                            (S(S(S(S(S(S Z)))))) Gas Low)
redPathOne | Like p86 <- p (ix 8) (iy 6)
           = --Pcons (p (ix 7) (iy 6))
               --      Back
               --      Pcons (p (ix 6) (iy 6))
               --            Next
               --            Pcons (p (ix 7) (iy 6))
               --                  Next
               Pcons p86
                     Stay
                     P0
