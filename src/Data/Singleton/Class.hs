{-# LANGUAGE
    KindSignatures
  #-}

module Data.Singleton.Class where

import Data.Functor.Identity (Identity (..))



class Extractable (f :: * -> *) where
  runSingleton :: f a -> a


instance Extractable ((,) e) where
  runSingleton (_,x) = x

instance Extractable ((,,) w s) where
  runSingleton (_,_,x) = x

instance Extractable Identity where
  runSingleton (Identity x) = x
