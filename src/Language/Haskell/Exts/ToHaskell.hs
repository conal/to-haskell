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
  ( ToHS(..), HDoc, Prec, showsPrecHS
  , appHS, ifHS, lamHS, letHS, tupleHS, infixHS, hsName, hsShow
  , ToHSPat(..)
  ) where

import Data.Functor ((<$>))
import Control.Applicative (liftA2,liftA3)
import Control.Monad ((>=>),mzero)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Pretty (prettyPrint)

type Prec = Int -- precedence level

type HDoc = Prec -> Exp

class ToHS a where
  toHS :: a -> HDoc

showsPrecHS :: ToHS a => Prec -> a -> ShowS
showsPrecHS p a s = prettyPrint (toHS a p) ++ s

-- Precedence of function application.
-- Hack: use 11 instead of 10 to avoid extraneous parens when a function
-- application is the left argument of a function composition.
appPrec :: Prec
appPrec = 11 -- was 10

appHS :: Binop HDoc
appHS f a p = hsParen (p > appPrec) $
              App (f appPrec) (a (appPrec+1))

ifHS :: Ternop HDoc
ifHS i t e p = hsParen (p > 0) $ If (i 0) (t 0) (e 0)

lamHS :: Pat -> Unop HDoc
lamHS pat d p = hsParen (p > 0) $
                Lambda noLoc [pat] (d 0)

letHS :: Pat -> Binop HDoc
letHS pat rhs body p = hsParen (p > 0) $ 
                       simpleLet pat (rhs 0) (body 0)

simpleLet :: Pat -> Binop Exp
simpleLet pat r b =
  mkLet (BDecls [PatBind noLoc pat Nothing (UnGuardedRhs r) (BDecls [])]) b

mkLet :: Binds -> Exp -> Exp
mkLet (BDecls ds) (Let (BDecls ds') e) = Let (BDecls (ds ++ ds')) e
mkLet bs          e                    = Let bs e

tupleHS :: [HDoc] -> HDoc
tupleHS ds _ = Tuple (map ($ 0) ds)

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
          InfixApp (a (lf q)) (QVarOp name) (b (rf q))
 where
   (lf,rf) = case assoc of
               AssocLeft  -> (incr, succ)
               AssocRight -> (succ, incr)
               AssocNone  -> (succ, succ)
   incr | extraParens = succ
        | otherwise   = id

extraParens :: Bool
extraParens = True

hsName :: String -> HDoc
hsName s _ = Var . UnQual . Ident $ s -- hack: for numbers etc

hsShow :: Show a => a -> HDoc
hsShow = hsName . show

noLoc :: SrcLoc
noLoc = SrcLoc "no file" 0 0

hsParen :: Bool -> Unop Exp
hsParen True  = Paren
hsParen False = id

allFixities :: [Fixity]
allFixities = baseFixities

class ToHSPat a where toHSPat :: a -> Pat

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

type Unop   a = a -> a
type Binop  a = a -> Unop a
type Ternop a = a -> Binop a
