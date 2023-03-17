{-# LANGUAGE OverloadedStrings #-}

module Examples where
{-
  (
    pIntListP,
    pListP,
    pArith,
    pStream,
    pSeq,
    pEither,
    pRepeat
  )
-}

import Types
import PrettyFreeST as PF
import qualified PrettyAlgST as PA

pSeq :: Protocol
pSeq = Protocol "Seq" ["x", "y"] [Constructor "Seq" [Argument Plus ((TyPVar "x"))
                                                    , Argument Plus ((TyPVar "y"))]]

pAlt :: Protocol
pAlt = Protocol "Alt" ["X", "Y"] [Constructor "Left" [Argument Plus ((TyPVar "X"))],
                                  Constructor "Right" [Argument Plus ((TyPVar "Y"))]]

pRep :: Protocol
pRep = Protocol "Rep" ["X"] [Constructor "Nil" [],
                             Constructor "Cons" [ Argument Plus ((TyPVar "X"))
                                                , Argument Plus (TyApp "Rep" [TyPVar "X"])]]

pIntList :: Protocol
pIntList = Protocol "IList" [] [Constructor "INil" [],
                                Constructor "ICons" [ Argument Plus (TyType (TyBase "Int"))
                                                    , Argument Plus (TyApp "IList" [])]]

tInt :: Type
tInt = TyBase "Int"

tUnit :: Type
tUnit = TyUnit

tpInt :: TyProto
tpInt = TyType tInt

tpUnit :: TyProto
tpUnit = TyType tUnit

tpIntList :: TyProto
tpIntList = TyApp "IList" []

t1 :: Type
t1 = TySession $ TyTransmit Input tpInt $ TyTransmit Input tpInt $ TyTransmit Output tpUnit $ TyEnd Input

t2 :: Type
t2 = TySession $ TyTransmit Input tpIntList $ TyTransmit Output tpIntList $ TyEnd Input

t3 :: Type
t3 = TySession $ TyTransmit Input (TyApp "Rep" [TyType tInt]) $ TyEnd Input

t4 :: Type
t4 = TySession $ TyTransmit Output (TyApp "Rep" [TyType t3]) $ TyEnd Input

ex1_4 :: IO ()
ex1_4 = putPretty $ prettyModule $ Module [pRep, pIntList] [t1,t2,t3,t4]

-- now something mutually recursive

pTree :: Protocol
pTree = Protocol "Tree" [] [Constructor "Tree" [Argument Plus tpInt, Argument Plus (TyApp "Forest" [])]]

pForest :: Protocol
pForest = Protocol "Forest" [] [Constructor "Nil" [],
                                Constructor "Cons" [Argument Plus (TyApp "Tree" []),
                                                    Argument Plus (TyApp "Forest" [])]]

tpTree :: Type
tpTree = TySession $ TyTransmit Input (TyApp "Tree" []) $ TyEnd Input

extpTree :: IO ()
extpTree = putPretty $ prettyModule $ Module [pTree, pForest] [tpTree]

-- same examples for AlgST

exPA :: IO ()
exPA = PA.putPretty (PA.prettyModule $ Module [pRep, pIntList, pTree, pForest] [t1, t2, t3, t4, tpTree])

-- protocol definitions from the paper including the toolkit of generic protocols

pIntListP, pListP, pAstP, pArith, pStream, pEither, pRepeat :: Protocol

pIntListP  = Protocol "IntListP" [] [Constructor "INil" []
                                    ,Constructor "ICons" [Argument Plus (TyType (TyBase "Int"))
                                                         ,Argument Plus (TyApp "IntListP" [])]]

pListP  = Protocol "ListP" ["x"] [Constructor "Nil" []
                                 ,Constructor "Cons" [Argument Plus ((TyPVar "x"))
                                                     ,Argument Plus (TyApp "ListP" [(TyPVar "x")])]]

pAstP   = Protocol "AstP" [] [Constructor "ConP" [Argument Plus (TyType (TyBase "Int"))]
                             ,Constructor "AddP" [Argument Plus (TyApp "AstP" [])
                                                 ,Argument Plus (TyApp "AstP" [])]]

pArith  = Protocol "Arith" [] [Constructor "Neg" [Argument Plus (TyType (TyBase "Int"))
                                                 ,Argument Minus (TyType (TyBase "Int"))]
                              ,Constructor "Add" [Argument Plus (TyType (TyBase "Int"))
                                                 ,Argument Plus (TyType (TyBase "Int"))
                                                 ,Argument Minus (TyType (TyBase "Int"))]]

pStream = Protocol "Stream" ["x"] [Constructor "Next" [Argument Plus ((TyPVar "x"))
                                                      ,Argument Plus (TyApp "Stream" [(TyPVar "x")])]]


pEither = Protocol "Either" ["x", "y"] [Constructor "Left" [Argument Plus ((TyPVar "x"))]
                                       ,Constructor "Right" [Argument Plus ((TyPVar "y"))]]

pRepeat = Protocol "Repeat" ["x"] [Constructor "Quit" []
                               ,Constructor "More" [Argument Plus ((TyPVar "x"))
                                                ,Argument Plus (TyApp "Repeat" [(TyPVar "x")])]]
