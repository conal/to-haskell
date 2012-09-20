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

module Language.NameSupply (Supply,supplyNew,Name,NameM,withNames) where

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

allNames :: [Name]

allNames = reverse <$> tail ns
 where
   ns = "" : [c : n | n <- ns, c <- ['a'..'z']]

-- allNames = reverse <$> tail ns
--  where
--    ns = "" : liftA2 (flip (:)) ns ['a'..'z']

type NameM = Supply Name

withNames :: NameM a -> a
withNames = flip evalState allNames
