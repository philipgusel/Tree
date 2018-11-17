module Tree where

import Data.Monoid
import Data.Foldable


data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)

instance Functor Tree where
        fmap _ Nil = Nil
        fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative Tree where
        pure a = Node a Nil Nil
        Node f fl fr <*> Node g gl gr = Node (f g) (fl <*> gl) (fr <*> gr)
        _ <*> Nil = Nil
        Nil <*> _ = Nil

instance Monad Tree where
        return a = pure a
        (Node a l r) >>= f = f a
        Nil >>= _ = Nil

instance Foldable Tree where
        fold Nil = mempty
        fold (Node a l r) = fold l `mappend` a `mappend` fold r

        foldMap f Nil = mempty
        foldMap f (Node a l r) = (foldMap f l) `mappend` (f a) `mappend` (foldMap f r)

        foldr f b Nil = b
        foldr f b (Node a l r) = foldr f (f a (foldr f b r)) l
