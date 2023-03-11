{-# LANGUAGE OverloadedStrings #-}

module Examples where

import Text.PrettyPrint

import Types
import PrettyFreeST as PF
import qualified PrettyAlgSt as PA

pSeq :: Protocol
pSeq = Protocol "Seq" ["X", "Y"] [Constructor "Seq" [Argument Plus (TyType (TyVar "X")), Argument Plus (TyType (TyVar "Y"))]]

pAlt :: Protocol
pAlt = Protocol "Alt" ["X", "Y"] [Constructor "Left" [Argument Plus (TyType (TyVar "X"))],
                                  Constructor "Right" [Argument Plus (TyType (TyVar "Y"))]]

pRep :: Protocol
pRep = Protocol "Rep" ["X"] [Constructor "Nil" [],
                             Constructor "Cons" [Argument Plus (TyType (TyVar "X")), Argument Plus (TyApp "Rep" [TyVar "X"])]]

pIntList :: Protocol
pIntList = Protocol "IList" [] [Constructor "INil" [],
                                Constructor "ICons" [Argument Plus (TyType (TyBase "Int")),
                                                     Argument Plus (TyApp "IList" [])]]

tInt :: Type
tInt = TyBase "Int"

tBool :: Type
tBool = TyBase "Bool"

tpInt :: TyProto
tpInt = TyType tInt

tpBool :: TyProto
tpBool = TyType tBool

tpIntList :: TyProto
tpIntList = TyApp "IList" []

t1 :: Type
t1 = TySession $ TyTransmit Input tpInt $ TyTransmit Input tpInt $ TyTransmit Output tpBool $ TyEnd Input

t2 :: Type
t2 = TySession $ TyTransmit Input tpIntList $ TyTransmit Output tpIntList $ TyEnd Input

t3 :: Type
t3 = TySession $ TyTransmit Input (TyApp "Rep" [tInt]) $ TyEnd Input

t4 :: Type
t4 = TySession $ TyTransmit Output (TyApp "Rep" [t3]) $ TyEnd Input

ex1_4 = putStrLn $ pretty $ prettyModule $ Module [pRep, pIntList] [t1,t2,t3,t4]

-- now something mutually recursive

pTree :: Protocol
pTree = Protocol "Tree" [] [Constructor "Tree" [Argument Plus tpInt, Argument Plus (TyApp "Forest" [])]]

pForest :: Protocol
pForest = Protocol "Forest" [] [Constructor "Nil" [],
                                Constructor "Cons" [Argument Plus (TyApp "Tree" []),
                                                    Argument Plus (TyApp "Forest" [])]]

tpTree :: Type
tpTree = TySession $ TyTransmit Input (TyApp "Tree" []) $ TyEnd Input

extpTree = putStrLn $ pretty $ prettyModule $ Module [pTree, pForest] [tpTree]

-- same examples for AlgST

exPA = putStrLn $ PA.pretty (PA.prettyModule $ Module [pRep, pIntList, pTree, pForest] [t1, t2, t3, t4, tpTree])

