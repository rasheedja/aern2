{-|
    Module      :  AERN2.RealFun.Classes
    Description :  Classes for real number function operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Classes for real number function operations
-}

module AERN2.RealFun.Classes
(
  HasDomain(..), CanApply(..), HasVars(..),
  HasConstFunctions
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import Data.Typeable

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

-- import AERN2.MP.Dyadic
-- import AERN2.MP.Ball

class HasDomain f where
  type Domain f
  getDomain :: f -> Domain f

class CanApply f x where
  type ApplyType f x
  {-| compute @f(x)@  -}
  apply :: f {-^ @f@ -} -> x {-^ @x@ -} -> ApplyType f x

class HasVars f where
  type Var f
  {-| the function @x@, ie the function that project the domain to the given variable @x@  -}
  varF ::
    f {-^ sample function with the same domain -}->
    Var f {-^ @x@ -} ->
    f

type HasConstFunctions t f = ConvertibleExactly t f
