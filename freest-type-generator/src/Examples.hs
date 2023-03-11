module Examples where

import Text.PrettyPrint

import Types
import PrettyFreeST as PF
import qualified PrettyAlgSt as PA

pSeq :: Protocol
pSeq = Protocol "Seq" ["X", "Y"] [Constructor "Seq" [Argument Plus (TyVar "X"), Argument Plus (TyVar "Y")]]

pAlt :: Protocol
pAlt = Protocol "Alt" ["X", "Y"] [Constructor "Left" [Argument Plus (TyVar "X")],
                                  Constructor "Right" [Argument Plus (TyVar "Y")]]

pRep :: Protocol
pRep = Protocol "Rep" ["X"] [Constructor "Nil" [],
                             Constructor "Cons" [Argument Plus (TyVar "X"), Argument Plus (TyApp "Rep" [TyVar "X"])]]

pIntList :: Protocol
pIntList = Protocol "IList" [] [Constructor "INil" [],
                                Constructor "ICons" [Argument Plus (TyBase "Int"),
                                                     Argument Plus (TyApp "IList" [])]]

tInt :: Type
tInt = TyBase "Int"

tBool :: Type
tBool = TyBase "Bool"

tpIntList :: Type
tpIntList = TyApp "IList" []

t1 :: Type
t1 = TySession Input tInt $ TySession Input tInt $ TySession Output tBool $ TyEnd Input

t2 :: Type
t2 = TySession Input tpIntList $ TySession Output tpIntList $ TyEnd Input

t3 :: Type
t3 = TySession Input (TyApp "Rep" [tInt]) $ TyEnd Input

t4 :: Type
t4 = TySession Output (TyApp "Rep" [t3]) $ TyEnd Input

ex1_4 = putStrLn $ pretty $ prettyModule $ Module [pRep, pIntList] [t1,t2,t3,t4]

-- now something mutually recursive

pTree :: Protocol
pTree = Protocol "Tree" [] [Constructor "Tree" [Argument Plus tInt, Argument Plus (TyApp "Forest" [])]]

pForest :: Protocol
pForest = Protocol "Forest" [] [Constructor "Nil" [],
                                Constructor "Cons" [Argument Plus (TyApp "Tree" []),
                                                    Argument Plus (TyApp "Forest" [])]]

tpTree :: Type
tpTree = TySession Input (TyApp "Tree" []) $ TyEnd Input

extpTree = putStrLn $ pretty $ prettyModule $ Module [pTree, pForest] [tpTree]

-- same examples for AlgST

exPA = putStrLn $ PA.pretty (PA.prettyModule $ Module [pRep, pIntList, pTree, pForest] [t1, t2, t3, t4, tpTree])

