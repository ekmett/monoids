-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid.Sugar
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Syntactic sugar for working with a 'Monoid' and 'Multiplicative' instances 
-- that conflicts with names from the "Prelude".
--
-- > import Prelude hiding ((+),(*),(^))
-- > import Data.Monoid.Sugar
--
-----------------------------------------------------------------------------
--
module Data.Monoid.Sugar
    ( (+)
    , (*)
    , (^) 
    ) where

import Prelude hiding ((*),(+),(^))
import Data.Monoid (Monoid, mappend)
import Data.Monoid.Multiplicative (Multiplicative, times, Log(..))
import qualified Data.Monoid.Combinators as Monoid

infixl 6 + 
infixl 7 *

(+) :: Monoid m => m -> m -> m 
(+) = mappend

(*) :: Multiplicative r => r -> r -> r
(*) = times

(^) :: (Multiplicative r, Integral b) => r -> b -> r
m ^ n =  getLog (Monoid.replicate (Log m) n)
