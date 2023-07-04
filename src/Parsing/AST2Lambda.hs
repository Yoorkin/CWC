module Parsing.AST2Lambda(translate) where

import qualified Parsing.AST as M
import qualified Lambda as L

translate :: M.Module -> L.Lexp
translate (Module [x:xs]) = 

transMexp2Lam :: M.Mexp -> L.Lambda
transMexp2Lam (M.Var x) = L.Var x
transMexp2Lam (M.Apply m1 m2) = L.App (transMexp2Lam m1) (transMexp2Lam m2)



