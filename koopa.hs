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
data NATTY :: * where
  Nat :: Natty n -> NATTY
nattyer :: Nat -> NATTY
nattyer Z = Nat Zy
nattyer (S n) = case nattyer n of Nat m -> Nat (Sy m)


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
data MATTY :: * where
  Mater :: Matty m -> MATTY
mattyer :: Material -> MATTY
mattyer Gas = Mater Gasy
mattyer Solid = Mater Solidy

data Clearance = Low | High | God
data Clearry :: Clearance -> * where
  Lowy  :: Clearry Low
  Highy :: Clearry High
  Gody  :: Clearry God
data CLEARRY :: * where
  Clear :: Clearry cl -> CLEARRY
clearryer :: Clearance -> CLEARRY
clearryer Low = Clear Lowy
clearryer High = Clear Highy
clearryer God = Clear Gody


data Position = Pos { getX      :: Nat
                    , getY      :: Nat
                    , matter :: Material
                    , clr    :: Clearance
                    }
data Positionny :: Position -> * where
  Posy :: Natty x -> Natty y -> Matty m -> Clearry cl
            -> Positionny (Pos x y m cl)
data POSITIONNY :: * where
  Posit :: Positionny p -> POSITIONNY
positionnyer :: Position -> POSITIONNY
positionnyer (Pos x y m cl) 
  | Nat xY <- nattyer x
  , Nat yY <- nattyer y
  , Mater mY <- mattyer m
  , Clear clY <- clearryer cl
  = Posit (Posy xY yY mY clY)

-- data CoClr :: Color -> Clearance -> * where
--   RedClr   :: CoClr c Low
--   GreenClr :: CoClr Green High

class CoClr (c :: Color) (cl :: Clearance)
instance CoClr c Low
instance CoClr Green High

data Follows :: Position -> Position -> Color -> * where
  Stay :: Follows (Pos x y Gas Low) (Pos x y Gas Low) c
  Next :: CoClr c cl => Follows (Pos (S x) y Gas cl) (Pos x y Gas Low) c
  Back :: CoClr c cl => Follows (Pos x y Gas cl) (Pos (S x) y Gas Low) c
  Fall :: Follows (Pos x y Gas cl) (Pos x (S y) Gas High) c

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
     -> POSITIONNY
p x y | Pos x' y' m' cl' <- mlookup y x exampleLevel
      , Nat xy <- nattyer x'
      , Nat yy <- nattyer y'
      , Mater my <- mattyer m'
      , Clear cly <- clearryer cl'
      = Posit (Posy xy yy my cly)


p01 :: Positionny (Pos Z (S Z) Solid God)
p01 = Posy Zy (Sy Zy) Solidy Gody
p11 :: Positionny (Pos (S Z) (S Z) Gas Low)
p11 = Posy (Sy Zy) (Sy Zy) Gasy Lowy
p21 :: Positionny (Pos (S(S Z)) (S Z) Gas Low)
p21 = Posy (Sy(Sy Zy)) (Sy Zy) Gasy Lowy
p22 :: Positionny (Pos (S(S Z)) (S(S Z)) Gas High)
p22 = Posy (Sy(Sy Zy)) (Sy(Sy Zy)) Gasy Highy
p23 :: Positionny (Pos (S(S Z)) (S(S(S Z))) Gas High)
p23 = Posy (Sy(Sy Zy)) (Sy(Sy(Sy Zy))) Gasy Highy
p24 :: Positionny (Pos (S(S Z)) (S(S(S(S Z)))) Gas High)
p24 = Posy (Sy(Sy Zy)) (Sy(Sy(Sy(Sy Zy)))) Gasy Highy
p31 :: Positionny (Pos (S(S(S Z))) (S Z) Gas Low)
p31 = Posy (Sy(Sy(Sy Zy))) (Sy Zy) Gasy Lowy
p34 :: Positionny (Pos (S(S(S Z))) (S(S(S(S Z)))) Gas Low)
p34 = Posy (Sy(Sy(Sy Zy))) (Sy(Sy(Sy(Sy Zy)))) Gasy Lowy
p41 :: Positionny (Pos (S(S(S(S Z)))) (S Z) Gas Low)
p41 = Posy (Sy(Sy(Sy(Sy Zy)))) (Sy Zy) Gasy Lowy
p44 :: Positionny (Pos (S(S(S(S Z)))) (S(S(S(S Z)))) Gas Low)
p44 = Posy (Sy(Sy(Sy(Sy Zy)))) (Sy(Sy(Sy(Sy Zy)))) Gasy Lowy
p51 :: Positionny (Pos (S(S(S(S(S Z))))) (S Z) Gas High)
p51 = Posy (Sy(Sy(Sy(Sy(Sy Zy))))) (Sy Zy) Gasy Highy
p54 :: Positionny (Pos (S(S(S(S(S Z))))) (S(S(S(S Z)))) Gas Low)
p54 = Posy (Sy(Sy(Sy(Sy(Sy Zy))))) (Sy(Sy(Sy(Sy Zy)))) Gasy Lowy
p55 :: Positionny (Pos (S(S(S(S(S Z))))) (S(S(S(S(S Z))))) Gas High)
p55 = Posy (Sy(Sy(Sy(Sy(Sy Zy))))) (Sy(Sy(Sy(Sy(Sy Zy))))) Gasy Highy
p56 :: Positionny (Pos (S(S(S(S(S Z))))) (S(S(S(S(S(S Z)))))) Gas High)
p56 = Posy (Sy(Sy(Sy(Sy(Sy Zy))))) (Sy(Sy(Sy(Sy(Sy(Sy Zy)))))) Gasy Highy
p66 :: Positionny (Pos (S(S(S(S(S(S Z)))))) (S(S(S(S(S(S Z)))))) Gas Low)
p66 = Posy (Sy(Sy(Sy(Sy(Sy(Sy Zy))))))
           (Sy(Sy(Sy(Sy(Sy(Sy Zy)))))) Gasy Lowy
p76 :: Positionny (Pos (S(S(S(S(S(S(S Z))))))) (S(S(S(S(S(S Z)))))) Gas Low)
p76 = Posy (Sy(Sy(Sy(Sy(Sy(Sy(Sy Zy)))))))
           (Sy(Sy(Sy(Sy(Sy(Sy Zy)))))) Gasy Lowy
p86 :: Positionny (Pos (S(S(S(S(S(S(S(S Z)))))))) (S(S(S(S(S(S Z)))))) Gas Low)
p86 = Posy (Sy(Sy(Sy(Sy(Sy(Sy(Sy(Sy Zy))))))))
           (Sy(Sy(Sy(Sy(Sy(Sy Zy)))))) Gasy Lowy

redPathOne :: Path Red (Pos (S(S(S(S(S(S(S Z)))))))
                            (S(S(S(S(S(S Z)))))) Gas Low)
                       (Pos (S(S(S(S(S(S(S(S Z))))))))
                            (S(S(S(S(S(S Z)))))) Gas Low)
redPathOne = Pcons p76 Back
           $ Pcons p66 Next
           $ Pcons p76 Next
           $ Pcons p86 Stay P0

redPathTwo :: Path Red (Pos (S(S Z))    (S Z) Gas Low)
                       (Pos (S(S(S Z))) (S Z) Gas Low)
redPathTwo = Pcons p21 Back
           $ Pcons p11 Next
           $ Pcons p21 Next
           $ Pcons p31 Next
           $ Pcons p41 Back
           $ Pcons p31 Stay P0

-- Because Stay only allows a position with material Gas to follow from
-- a position with material Gas, this is a type error
-- redNoPathOne :: Path Red (Pos (S Z) (S Z) Gas Low)
--                          (Pos Z (S Z) Solid God)
-- redNoPathOne = Pcons p11 Back
--              $ Pcons p01 Stay P0

-- Red Koopa Troopa can't step into a wall
-- This is for the same reason as in redNoPathOne but with the Back
-- constructor
-- redNoPathTwo :: Path Red (Pos (S Z) (S Z) Gas Low)
--                          (Pos Z (S Z) Solid God)
-- redNoPathTwo = Pcons p11 Back P0

-- Red Koopa Troopa can't step into air
-- Here the problem is that there is no instance for CoClr Red High which
-- is necessary for Next, however the solution GHC suggests is to add it
-- while it was actually not defined on purpose
-- redNoPathThree :: Path Red (Pos (S(S(S(S Z)))) (S Z) Gas Low)
--                            (Pos (S(S(S(S(S Z))))) (S Z) Gas High)
-- redNoPathThree = Pcons p41 Next P0

-- Any path that is valid for red Koopa Troopas, is also valid for green
-- Koopa Troopas because we did not constrain Koopa Troopas to only turn
-- when there is an obstacle
greenPathOne :: Path Green (Pos (S(S(S(S(S(S(S Z)))))))
                                (S(S(S(S(S(S Z)))))) Gas Low)
                           (Pos (S(S(S(S(S(S(S(S Z))))))))
                                (S(S(S(S(S(S Z)))))) Gas Low)
greenPathOne = Pcons p76 Back
             $ Pcons p66 Next
             $ Pcons p76 Next
             $ Pcons p86 Stay P0

greenPathTwo :: Path Green (Pos (S(S(S(S(S(S(S Z)))))))
                                (S(S(S(S(S(S Z)))))) Gas Low)
                           (Pos (S(S(S(S(S Z))))) Z Gas Low)
greenPathTwo = Pcons p76 Back
             $ Pcons p66 Back
             $ Pcons p56 Fall
             $ Pcons p55 Fall
             $ Pcons p54 Back
             $ Pcons p44 Back
             $ Pcons p34 Back
             $ Pcons p24 Fall
             $ Pcons p23 Fall
             $ Pcons p22 Fall
             $ Pcons p21 Back
             $ Pcons p11 Next
             $ Pcons p21 Next
             $ Pcons p31 Next
             $ Pcons p41 Next
             $ Pcons p51 Fall P0

-- Green Koopa Troopa can't step into a wall
-- Exactly the same as for redNoPathTwo
-- greenNoPathOne :: Path Green (Pos (S Z) (S Z) Gas Low)
--                              (Pos Z (S Z) Solid God)
-- greenNoPathOne = Pcons p11 Back P0
