{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Multiplicative
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable (but instances use MPTCs)
--
-- When dealing with a 'Ring' or other structure, you often need a pair of 
-- 'Monoid' instances that are closely related. Making a @newtype@ for one
-- is unsatisfying and yields an unnatural programming style. 
--
-- A 'Multiplicative' is a 'Monoid' that is intended for use in a scenario
-- that can be extended to have another 'Monoid' slot in for addition. This
-- enables one to use common notation.
--
-- Any 'Multiplicative' can be turned into a 'Monoid' using the 'Log' wrapper.
--
-- Any 'Monoid' can be turned into a 'Multiplicative' using the 'Exp' wrapper.
--
-- Instances are supplied for common Monads of Monoids, in a fashion 
-- which can be extended if the 'Monad' is a 'MonadPlus' to yield a 'RightSemiNearRing'
--
-- Instances are also supplied for common Applicatives of Monoids, in a
-- fashion which can be extended if the 'Applicative' is 'Alternative' to
-- yield a 'RightSemiNearRing'
-----------------------------------------------------------------------------

module Data.Monoid.Multiplicative 
    ( Multiplicative
    , one, times
    -- * Multiplicative to Monoid
    , Log(Log, getLog)
    -- * Monoid to Multiplicative
    , Exp(Exp, getExp)
    ) where

import Control.Applicative
import Control.Monad (liftM2)
import Data.Monoid (Monoid, mappend, mempty, Dual(..))
import Data.Generator
import Data.Monoid.Self
import Data.Ratio

import Data.FingerTree
import Data.Sequence (Seq)

class Multiplicative m where
    one :: m
    times :: m -> m -> m

instance Multiplicative m => Multiplicative (Dual m) where
    one = Dual one
    Dual x `times` Dual y = Dual (y `times` x)

-- | Convert a 'Multiplicative' into a 'Monoid'. Mnemonic: @Log a + Log b = Log (a * b)@
data Log m = Log { getLog :: m }

instance Multiplicative m => Monoid (Log m) where
    mempty = Log one
    Log a `mappend` Log b = Log (a `times` b)

-- | Convert a 'Monoid' into a 'Multiplicative'. Mnemonic: @Exp a * Exp b = Exp (a + b)@
data Exp m = Exp { getExp :: m }

instance Monoid m => Multiplicative (Exp m) where
    one = Exp mempty
    Exp a `times` Exp b = Exp (a `mappend` b)

instance Multiplicative m => Multiplicative (Self m) where
    one = Self one  
    Self a `times` Self b = Self (a `times` b)

instance Monoid m => Multiplicative [m] where
    one = return mempty
    times = liftM2 mappend

instance Monoid m => Multiplicative (Maybe m) where
    one = return mempty
    times = liftM2 mappend
instance Monoid n => Multiplicative (IO n) where
    one = return mempty
    times = liftM2 mappend

-- Applicative instances
instance Monoid n => Multiplicative (ZipList n) where
    one = pure mempty
    times = liftA2 mappend

instance Monoid m => Multiplicative (Const m a) where
    one = pure undefined
    times = liftA2 undefined

-- Numeric instances
instance Multiplicative Int where
    one = 1
    times = (*)

instance Multiplicative Integer where
    one = 1
    times = (*)

instance Integral m => Multiplicative (Ratio m) where
    one = 1
    times = (*)

instance Monoid m => Multiplicative (Seq m) where
    one = return mempty
    times = liftM2 mappend

-- not quite be a Monad in Haskell
instance (Measured v m, Monoid m) => Multiplicative (FingerTree v m) where
    one = singleton mempty
    xss `times` yss = getSelf $ mapReduce (flip fmap' yss . mappend) xss
