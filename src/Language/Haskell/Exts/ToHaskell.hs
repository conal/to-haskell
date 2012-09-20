-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.ToHaskell
-- Copyright   :  (c) 2012 Tabula, Inc.
-- 
-- Maintainer  :  conal@tabula.com
-- Stability   :  experimental
-- 
-- Conversion to Haskell code, plus utilities
----------------------------------------------------------------------

module Language.Haskell.Exts.ToHaskell
  ( ToHaskell(..), HDoc, showsPrecHS
  , appHS, lamHS, letHS, tupleHS, infixHS, hsName -- , opName
  ) where

import Data.Functor ((<$>))
import Control.Applicative (liftA2)
import Control.Monad ((>=>),mzero)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Pretty (prettyPrint)

import Language.NameSupply

type Prec = Int -- precedence level

type HDoc = Prec -> NameM Exp

class ToHaskell a where
  toHS :: a -> HDoc

showsPrecHS :: ToHaskell a => Prec -> a -> ShowS
showsPrecHS p a s = prettyPrint (withNames (toHS a p)) ++ s

-- Precedence of function application.
-- Hack: use 11 instead of 10 to avoid extraneous parens when a function
-- application is the left argument of a function composition.
appPrec :: Prec
appPrec = 11 -- was 10

appHS :: Binop HDoc
appHS f a p = hsParen (p >= appPrec) $
              liftA2 App (f (appPrec+1)) (a appPrec)

lamHS :: Pat -> Unop HDoc
lamHS pat d p = hsParen (p > 0) $
                Lambda noLoc [pat] <$> d 0

letHS :: Pat -> Binop HDoc
letHS pat rhs body p = hsParen (p > 0) $ 
                       liftA2 (simpleLet pat) (rhs 0) (body 0)

simpleLet :: Pat -> Binop Exp
simpleLet pat r b =
  Let (BDecls [PatBind noLoc pat Nothing (UnGuardedRhs r) (BDecls [])]) b


tupleHS :: [HDoc] -> HDoc
tupleHS ds _ = Tuple <$> mapM ($ 0) ds

-- opSym :: String -> QOp
-- opSym = QVarOp . UnQual . Symbol

infixHS :: String -> Maybe (Binop HDoc)
infixHS = (fmap.fmap) infixHS' fixity

-- type Fixity' = (Assoc,Int,QOp)

fixity :: String -> Maybe Fixity
fixity = flip M.lookup fixityMap

fixityMap :: M.Map String Fixity
fixityMap = M.fromList (keyed <$> allFixities)
 where
   keyed :: Fixity -> (String,Fixity)
   keyed f@(Fixity _ _ q) = (prettyPrint q, f)

-- data QName = Qual ModuleName Name | UnQual Name | Special SpecialCon

infixHS' :: Fixity -> Binop HDoc
infixHS' (Fixity assoc q name) a b p =
  hsParen (p > q) $
          liftA2 (flip InfixApp (QVarOp name)) (a (lf q)) (b (rf q))
 where
   (lf,rf) = case assoc of
               AssocLeft  -> (incr, succ)
               AssocRight -> (succ, incr)
               AssocNone  -> (succ, succ)
   incr | extraParens = succ
        | otherwise   = id

-- TODO: Look up fixities from Language.Haskell.Exts.Fixity

extraParens :: Bool
extraParens = True

-- TODO: Refactor. hsApp, hsApp1, hsOp2, .... Exp utilities.

hsName :: String -> HDoc
hsName s _ = return (Var . UnQual . Ident $ s) -- hack: for numbers etc


noLoc :: SrcLoc
noLoc = SrcLoc "no file" 0 0

hsParen :: Bool -> Unop (NameM Exp)
hsParen needsParens = fmap (if needsParens then Paren else id)

allFixities :: [Fixity]
allFixities = baseFixities

{-
-- Convert simple function names to operator names

opName :: Unop String
opName s = fromMaybe (tickify s) (dropEnds '(' ')' s)

tickify :: Unop String
tickify = ("`" ++) . (++ "`")

dropEnds :: Eq a => a -> a -> [a] -> Maybe [a]
dropEnds a b = dropRev a >=> dropRev b

dropRev :: Eq a => a -> [a] -> Maybe [a]
dropRev x = fmap reverse . dropP
 where
   dropP (a:as) | a==x = return as
   dropP _             = mzero
-}

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

type Unop  a = a -> a
type Binop a = a -> Unop a
