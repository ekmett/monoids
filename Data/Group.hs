----------------------------------------------------------------------------
-- |
-- Module     : Data.Group
-- Copyright  : 2007-2009 Edward Kmett
-- License    : BSD
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Extends 'Monoid' to support 'Group' operations
-----------------------------------------------------------------------------

module Data.Group 
    ( Group
    , gnegate
    , gsubtract
    , minus
    , MultiplicativeGroup
    , over
    , under
    , grecip
    ) where

import Data.Monoid (Monoid, Sum(..), Product(..), Dual(..))
import Data.Monoid.Additive (plus, zero)
import Data.Monoid.Multiplicative (Multiplicative, one, times, Log(..), Exp(..))
import Data.Monoid.Self (Self(Self,getSelf))

infixl 6 `minus`

-- | Minimal complete definition: 'gnegate' or 'minus'
class Monoid a => Group a where
    -- additive inverse
    gnegate :: a -> a
    minus :: a -> a -> a
    gsubtract :: a -> a -> a 

    gnegate = minus zero
    a `minus` b = a `plus` gnegate b 
    a `gsubtract` b = gnegate a `plus` b

instance Num a => Group (Sum a) where
    gnegate = Sum . negate . getSum
    Sum a `minus` Sum b = Sum (a - b)
    
instance Fractional a => Group (Product a) where
    gnegate = Product . negate . getProduct
    Product a `minus` Product b = Product (a / b)
    
instance Group a => Group (Dual a) where
    gnegate = Dual . gnegate . getDual

instance Group a => Group (Self a) where
    gnegate = Self . gnegate . getSelf
    Self a `minus` Self b = Self (a `minus` b)

-- | Minimal definition over or grecip
class Multiplicative g => MultiplicativeGroup g where
    -- | @x / y@
    over :: g -> g -> g
    -- | @x \ y@
    under :: g -> g -> g
    grecip :: g -> g

    x `under` y = grecip x `times` y
    x `over` y = x `times` grecip y
    grecip x = one `over` x

instance MultiplicativeGroup g => Group (Log g) where
    Log x `minus` Log y = Log (x `over` y)
    Log x `gsubtract` Log y = Log (x `under` y)
    gnegate (Log x) = Log (grecip x)

instance Group g => MultiplicativeGroup (Exp g) where
    Exp x `over` Exp y = Exp (x `minus` y)
    Exp x `under` Exp y = Exp (x `gsubtract` y)
    grecip (Exp x) = Exp (gnegate x)

instance MultiplicativeGroup g => MultiplicativeGroup (Self g) where
    Self x `over` Self y = Self (x `over` y)
    Self x `under` Self y = Self (x `under` y)
    grecip (Self x) = Self (grecip x)

instance MultiplicativeGroup a => MultiplicativeGroup (Dual a) where
    grecip = Dual . grecip . getDual

