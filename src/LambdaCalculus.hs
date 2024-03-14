{-# LANGUAGE LambdaCase #-}

module LambdaCalculus where

import Control.Monad.Except
import qualified Data.Map as Map
import Fun.Abs

data Closure
  = VClosure Env Ident Expr
  deriving (Show)

type Bindings = Map.Map Ident Expr
type Env = Map.Map Ident Closure

data Cxt = Cxt
  { cxtSig :: Bindings
  , cxtEnv :: Env
  }

churchDefs :: [(Ident, Expr)]
churchDefs =
  [
    ( Ident "zero"
    , Lambda
        (Ident "f")
        ( Lambda
            (Ident "x")
            (Var (Ident "x"))
        )
    )
  ,
    ( Ident "suc"
    , Lambda
        (Ident "n")
        ( Lambda
            (Ident "f")
            ( Lambda
                (Ident "x")
                ( App
                    (Var (Ident "f"))
                    ( App
                        ( App
                            (Var (Ident "n"))
                            (Var (Ident "f"))
                        )
                        (Var (Ident "x"))
                    )
                )
            )
        )
    )
  ,
    ( Ident "+"
    , Lambda
        (Ident "j")
        ( Lambda
            (Ident "k")
            ( Lambda
                (Ident "f")
                ( Lambda
                    (Ident "x")
                    ( App
                        (App (Var (Ident "j")) (Var (Ident "f")))
                        ( App
                            (App (Var (Ident "k")) (Var (Ident "f")))
                            (Var (Ident "x"))
                        )
                    )
                )
            )
        )
    )
  ]
toChurch :: Integer -> Expr
toChurch n
  | n <= 0 = Var (Ident "zero")
  | otherwise = App (Var (Ident "suc")) (toChurch (n - 1))


-- | Entry point: Program computes a number.
interpret :: Program -> Except String Closure
interpret (Prog defs (DMain mainExp)) = do
  -- when (strategy == CallByName) $ throwError "call-by-name"
  eval cxt mainExp
 where
  cxt =
    Cxt
      { cxtSig = Map.fromList $ map (\(DDef f xs e) -> (f, mkDef xs e)) defs
      , cxtEnv = Map.empty
      }

  mkDef :: [Ident] -> Expr -> Expr
  mkDef xs e = foldr Lambda e xs

eval :: Cxt -> Expr -> Except String Closure
eval cxt = \case
  Var x -> do
    case Map.lookup x (cxtEnv cxt) of
      Just (VClosure env x e) -> eval (cxt{cxtEnv = env}) e
      Nothing -> case Map.lookup x (cxtSig cxt) of
        Just e -> eval (cxt{cxtEnv = Map.empty}) e
        Nothing -> throwError ("unbound variable" ++ show x)
  Lambda x e -> pure (VClosure (cxtEnv cxt) x e)
  App operator operand -> do
    operator' <- eval cxt operator
    case operator' of
      VClosure env head body -> do
        operator' <- eval cxt (Lambda head operand)
        eval (cxt{cxtEnv = Map.insert head operator' env}) body