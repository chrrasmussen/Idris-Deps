module Data.Tree

%default total
%access public export


mutual
  record Tree a where
    constructor Node
    rootLabel : a
    subForest : Forest a

  Forest : Type -> Type
  Forest a = List (Tree a)


Eq a => Eq (Tree a) where
  (Node rootLabel1 subForest1) == (Node rootLabel2 subForest2) =
    rootLabel1 == rootLabel2 &&
     isSubForestsEqual subForest1 subForest2
      where
        isSubForestsEqual : Forest a -> Forest a -> Bool
        isSubForestsEqual [] [] = True
        isSubForestsEqual [] (y :: ys) = False
        isSubForestsEqual (x :: xs) [] = False
        isSubForestsEqual (x :: xs) (y :: ys) = x == y && isSubForestsEqual xs ys

Functor Tree where
  map func (Node rootLabel subForest) =
    Node (func rootLabel) (mapSubForest subForest)
      where
        mapSubForest : Forest a -> Forest b
        mapSubForest [] = []
        mapSubForest (x :: xs) = map func x :: mapSubForest xs

Show a => Show (Tree a) where
  show (Node rootLabel subForest) =
    "Node " ++ show rootLabel ++ " " ++ showSubForest subForest
      where
        showRestOfSubForest : Forest a -> String
        showRestOfSubForest [] = ""
        showRestOfSubForest (x :: xs) = ", " ++ show x ++ showRestOfSubForest xs

        showSubForest : Forest a -> String
        showSubForest [] = "[]"
        showSubForest (x :: xs) = "[" ++ show x ++ showRestOfSubForest xs ++ "]"
