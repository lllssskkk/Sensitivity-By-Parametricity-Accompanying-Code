{-# LANGUAGE LambdaCase #-}

module NbELambda where

import Control.Monad.Except
import qualified Data.Map as Map
import Fun.Abs

data Neutral = NVar Ident | NApp Neutral Normal
  deriving (Show)

data Normal
  = Closure Env Ident Expr
  | Neutral Neutral
  deriving (Show)

type Bindings = Map.Map Ident Expr
type Env = Map.Map Ident Normal

data Cxt = Cxt
  { cxtSig :: Bindings
  , cxtEnv :: Env
  }

-- | Entry point: Program computes a number.
interpret :: Program -> Except String Expr
interpret (Prog defs (DMain mainExp)) = do
  val <- eval cxt mainExp
  -- pure val
  reify cxt (map fst $ Map.toList $ cxtSig cxt) val
 where
  cxt =
    Cxt
      { cxtSig = Map.fromList $ map (\(DDef f xs e) -> (f, mkDef xs e)) defs
      , cxtEnv = Map.empty
      }

  mkDef :: [Ident] -> Expr -> Expr
  mkDef xs e = foldr Lambda e xs

eval :: Cxt -> Expr -> Except String Normal
eval cxt = \case
  Var x -> do
    case Map.lookup x (cxtEnv cxt) of
      Just (Neutral n) -> pure (Neutral n)
      Just (Closure env _x e) -> eval (cxt{cxtEnv = env}) e
      Nothing -> case Map.lookup x (cxtSig cxt) of
        Just e -> eval (cxt{cxtEnv = Map.empty}) e
        Nothing -> throwError ("unbound variable" ++ show x)
  Lambda x e -> pure (Closure (cxtEnv cxt) x e)
  App operator operand -> do
    operator' <- eval cxt operator
    case operator' of
      Neutral n -> do
        operand' <- eval cxt operand
        pure (Neutral (NApp n operand'))
      Closure env head body -> do
        operand' <- eval cxt (Lambda head operand)
        eval (cxt{cxtEnv = Map.insert head operand' env}) body

-- const = \x -> \f -> x;
-- const id1 id2
-- app (app (const id1)) id2

reify :: Cxt -> [Ident] -> Normal -> Except String Expr
reify _cxt _used (Neutral (NVar x)) = pure (Var x)
reify cxt used (Neutral (NApp neu v)) = do
  neu' <- reify cxt used (Neutral neu)
  v' <- reify cxt used v
  pure (App neu' v')
reify cxt used (Closure env x expr) = do
  let x' = freshen used x
  bodyNorm <- eval cxt{cxtEnv = Map.insert x (Neutral (NVar x')) env} expr
  bodyExpr <- reify cxt (x' : used) bodyNorm
  pure (Lambda x' bodyExpr)

freshen :: [Ident] -> Ident -> Ident
freshen used x
  | x `elem` used = freshen used (nextName x)
  | otherwise = x
 where
  nextName :: Ident -> Ident
  nextName (Ident y) = Ident $ y ++ "'"
