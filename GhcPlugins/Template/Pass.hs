{-# LANGUAGE PatternGuards #-}

module GhcPlugins.Template.Pass (transformProgram) where

import GhcPlugins

import Control.Monad
import Data.Generics

import GhcPlugins.Template.Annotation

-- ModGuts has a lot of fields, but mg_binds contains the top-level
-- bindings.  Defined in 'compiler/main/HscTypes.lhs'
transformProgram :: ModGuts -> CoreM ModGuts
transformProgram guts = do
  newBinds <- mapM (transformFunc guts) (mg_binds guts)
  return $ guts { mg_binds = newBinds }

-- CoreBind = Bind CoreBndr, which are either recursive or non-recursive.
-- Defined in 'compiler/coreSyn/CoreSyn.lhs', CoreBndr is a Var which
-- is defined in 'compiler/basicTypes/Var.lhs'
transformFunc :: ModGuts -> CoreBind -> CoreM CoreBind
transformFunc guts x = do
  b <- shouldTransformBind guts x
  if b
    then everywhereM (mkM transformExpr) x -- mkM/everywhereM are from 'syb'
    else return x

shouldTransformBind guts (NonRec b _) = shouldTransform guts b
shouldTransformBind guts (Rec bs) = or `liftM` mapM (shouldTransform guts . fst) bs

-- CoreExpr = Expr CoreBndr, which is the meat of Core.  Defined in
-- 'compiler/coreSyn/CoreSyn.lhs'.  The sample code here is just a
-- very verbose identity transformation.  Recall CoreBndr is a Var.
--
-- Note: we're using everywhereM to invoke this function, so you do
-- *not* need to make recursive subcalls, they will automatically be
-- made for you.  If you need a traversal that terminates early,
-- try some of the other functions in Data.Generics.Schemes
--
-- Things which you might need to do:
--      * Allocate a fresh name:  use mkSysLocalM, e.g.
--        mkSysLocalM (fsLit "somePrefix") typeOfExpr
--      * Determine the type of an expression: exprType
transformExpr :: CoreExpr -> CoreM CoreExpr
-- See 'Id'/'Var' in 'compiler/basicTypes/Var.lhs' (note: it's opaque)
transformExpr e@(Var x) | isTyVar x    = return e
                        | isTcTyVar x  = return e
                        | isLocalId x  = return e
                        | isGlobalId x = return e
-- See 'Literal' in 'compiler/basicTypes/Literal.lhs'
transformExpr e@(Lit l)     = return e
transformExpr e@(App e1 e2) = return e
transformExpr e@(Lam x e1)   = return e
-- b is a Bind CoreBndr, which is the same as CoreBind
transformExpr e@(Let b e1)   = return e
-- Remember case in core is strict!
transformExpr e@(Case e1 b t as) = return e
-- XXX These are pretty esoteric...
transformExpr e@(Cast e1 c)  = return e
transformExpr e@(Tick t e1)  = return e
transformExpr e@(Type t)    = return e
transformExpr e@(Coercion c) = return e

shouldTransform :: ModGuts -> CoreBndr -> CoreM Bool
shouldTransform guts bndr = do
  l <- annotationsOn guts bndr :: CoreM [Template]
  return $ not (null l)

annotationsOn :: Data a => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  anns <- getAnnotations deserializeWithData guts
  return $ lookupWithDefaultUFM anns [] (varUnique bndr)
