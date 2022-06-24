{-# LANGUAGE NoImplicitPrelude #-}

import Prelude hiding (Functor, fmap)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

data Box a = BoxCons a
  deriving (Show)

instance Functor Box where
  fmap f (BoxCons a) = BoxCons (f a)


main = putStrLn (show (fmap (\x -> x + 1) (BoxCons 2)))