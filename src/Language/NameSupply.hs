{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  Language.NameSupply
-- Copyright   :  (c) 2012 Tabula, Inc.
-- 
-- Maintainer  :  conal@tabula.com
-- Stability   :  experimental
-- 
-- Supply monad
----------------------------------------------------------------------

module Language.NameSupply
  ( Supply,supplyNew,allNames,Name,NameM,withNames,withAllNames
  ) where

-- TODO: explicit exports

import Data.Functor ((<$>))
import Control.Monad.Trans.State

type Supply x = State [x]

-- | Generate a new variable name
supplyNew :: Supply x x
supplyNew = do x:xs' <- get
               put xs'
               return x

type Name = String

-- | All names made purely of lower-case alphabetic characters, ordered by
-- increasing size (starting with one) and alphabetically within each size.
allNames :: [Name]
allNames = reverse <$> tail ns
 where
   ns = "" : [c : n | n <- ns, c <- ['a'..'z']]

-- allNames = reverse <$> tail ns
--  where
--    ns = "" : liftA2 (flip (:)) ns ['a'..'z']

type NameM = Supply Name

withNames :: [Name] -> NameM a -> a
withNames = flip evalState

withAllNames :: NameM a -> a
withAllNames = withNames allNames
