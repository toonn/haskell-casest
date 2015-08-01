{-
    Abstract Data Type for Ordered Schemas
                 Toon Nolten

    Since a schema is nothing more than a
       set, maybe use RedBlackTree.hs?
-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

module OrderedSchema where

import Universe
import Data.List (sort, insert)

data Bottom

type family So (b :: Bool)
type instance So True = ()
type instance So False = Bottom

type SchemaDescription = String

type Attribute = (String, U)


data Schema = Sorted [Attribute]

mkSchema :: [Attribute] -> Schema
mkSchema xs = Sorted (sort xs)

expandSchema :: Attribute -> Schema -> Schema
expandSchema x (Sorted xs) = Sorted (insert x xs)

schemify :: SchemaDescription -> Schema
schemify _sdesc = undefined


disjoint :: Schema -> Schema -> Bool
disjoint (Sorted schema1) (Sorted schema2) = disjoint' schema1 schema2
  where 
    disjoint' :: [Attribute] -> [Attribute] -> Bool
    disjoint' []       _       = True
    disjoint' _        []      = True
    disjoint' (x : xs) (y : ys)
      | x <  y = disjoint' xs (y : ys)
      | x == y = False
      | x >  y = disjoint' (x : xs) ys

sub :: Schema -> Schema -> Bool
sub (Sorted littleSchema) (Sorted bigSchema) = sub' littleSchema bigSchema
  where
    sub' :: [Attribute] -> [Attribute] -> Bool
    sub' [] _ = True
    sub' _ [] = False
    sub' (x : xs) (y : ys)
      | x <  y = False
      | x == y = sub' xs ys
      | x >  y = sub' (x : xs) ys

same :: Schema -> Schema -> Bool
same (Sorted schema1) (Sorted schema2) = same' schema1 schema2
  where
    same' :: [Attribute] -> [Attribute] -> Bool
    same' [] [] = True
    same' ((nm1, ty1) : xs) ((nm2, ty2) : ys) =
      nm1 == nm2 && ty1 == ty2 && same' xs ys
    same' _ _ = False

--occurs :: String -> Schema -> Bool
--occurs nm (Sorted s) = nm `elem` map fst s
--
--type family Occurs (nm :: String) (s :: Schema) :: Bool
--
--class Occurs (nm :: String) (s :: Schema) where
--instance Occurs nm ((nm,u) : s') where

lookup :: String -> Schema -> So (occurs nm s) -> U
lookup nm (Sorted s) _p = lookup' nm s
  where
    lookup' :: String -> [Attribute] -> U
    lookup' _ [] = undefined
    lookup' nm' ((name, typ) : s')
      | nm == name = typ
      | otherwise  = lookup' nm' s'

append :: Schema -> Schema -> Schema
append (Sorted s) (Sorted s') = mkSchema (s ++ s')
