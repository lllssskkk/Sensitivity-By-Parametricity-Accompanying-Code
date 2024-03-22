{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Snippet from Nachiappan's code

module NbETypedLambda where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass hiding (pPrintList)
import Prelude hiding ((<>))

import Control.Monad.State

data Exp a where
  Var :: Int -> Exp a
  Lam :: (NBE a, NBE b) => (Exp a -> Exp b) -> Exp (a -> b)
  App :: (NBE a) => Exp (a -> b) -> Exp a -> Exp b
  --
  ConsN :: Int -> Exp Int
  Add :: Exp Int -> Exp Int -> Exp Int

type Id = Int

-- Type class for reify & reflect
class NBE obj where
  type Host obj :: *
  reify :: Host obj -> Exp obj
  reflect :: Exp obj -> Either (Exp obj) (Host obj)

instance NBE Int where
  type Host Int = Int
  -- This type already considers being suspended
  reify n = ConsN n
  reflect = eval

--   reflect  exp   = ( reflect (Fst exp), reflect (Snd exp) )

instance (NBE a, NBE b) => NBE (a -> b) where
  type Host (a -> b) = Either (Exp a) (Host a) -> Either (Exp b) (Host b)

  -- \^ This is to pass the result of eval to f
  -- \^^ This is to analyze there
  -- is no Leftbolic variables
  -- before calling to reify
  reify f = Lam $ \e ->
    case f (eval e) of
      Left e -> e
      Right h -> reify h
  reflect f = eval f 
-- reflect expF  = \eitherA ->
--                    case eitherA of
--                         Left  expA  -> Left  $ App expF expA
--                         Right hostA -> Right $ reflect (App expF (reify hostA))

-- We can do pattern matching at eval
eval :: forall obj. (NBE obj) => Exp obj -> Either (Exp obj) (Host obj)
eval v@(Var id) = Left v
eval (Lam f) = Right $ \hostA -> case hostA of
  Left e -> eval $ f e
  Right h -> eval $ f (reify h)
eval (App expF exp) =
  case (eval expF, eval exp) of
    (Right f, arg) -> f arg
    (Left f, Left arg) -> Left (App f arg)
    (Left f, Right arg) -> Left (App f (reify arg))
eval (ConsN n) = Right n
eval (Add exp1 exp2) =
  case (eval exp1, eval exp2) of
    (Left e, Left e') -> Left (Add e e')
    (Left e, Right x) -> Left (Add e (ConsN x))
    (Right x, Left e) -> Left (Add (ConsN x) e)
    (Right n, Right m) -> Right (n + m)

norm :: (NBE obj) => Exp obj -> Exp obj
norm exp = case eval exp of
  Left e -> e
  Right h -> reify h

pExp :: Exp a -> StateT Id IO Doc
pExp (Var id) = return $ pVar id
pExp (Lam f) = do
  n <- get
  put (n + 1)
  body <- pExp (f (Var n))
  return $ lbrace <> backslash <> pVar n <+> dot <+> body <> rbrace
pExp (App expF expA) =
  (\f a -> f <+> char '@' <+> a)
    <$> (pExp expF)
    <*> (pExp expA)
-- Primitives
pExp (ConsN n) = return $ text $ show n
pExp (Add expA expB) =
  (\a b -> a <+> char '+' <+> b)
    <$> pExp expA
    <*> pExp expB
pVar id = text "Var" <> lbrack <> text (show id) <> rbrack
dot = text "->"
backslash = char $ toEnum 92

see exp = evalStateT (pExp exp) 0 >>= putStrLn . show

id :: Exp (Int -> Int)
id = Lam (\e -> e)

add :: Exp (Int -> Int -> Int)
add = Lam $ \e1 -> Lam $ \e2 -> Add e1 e2

example :: Exp ((Int -> Int) -> Int)
example = Lam $ \f -> App f (App (App add $ ConsN 1) $ ConsN 10)

-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# HLINT ignore "Use lambda-case" #-}
-- {-# HLINT ignore "Redundant lambda" #-}
-- {-# HLINT ignore "Eta reduce" #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}

-- module NbETypedLambda where

-- import Text.PrettyPrint
-- import Text.PrettyPrint.HughesPJClass hiding (pPrintList)
-- import Prelude hiding ((<>))

-- import Control.Monad.State
-- import Data.Kind (Type)
-- import qualified Data.Map as Map
-- import Debug.Trace
-- import GHC.Float (leDouble)
-- import Text.Read.Lex (expect)

-- data Exp a where
--   Var :: String -> Exp a
--   Lam :: (NBE a, NBE b) => (Exp a -> Exp b) -> Exp (a -> b)
--   App :: (NBE a) => Exp (a -> b) -> Exp a -> Exp b
--   ConsN :: Int -> Exp Int
--   Add :: Exp Int -> Exp Int -> Exp Int

-- example :: Exp ((Int -> Int) -> Int)
-- example = Lam $ \f -> App f (Add (ConsN 1) (ConsN 1))

-- instance Show (Exp a) where
--   show :: Exp a -> String
--   show (ConsN n) = show n
--   show (Lam f) = ""

-- data Ne a where
--   NVar :: (NBE a) => String -> Ne a
--   NApp :: (NBE a) => Ne (a -> b) -> Nf a -> Ne b

-- data Nf a where
--   NInt :: Int -> Nf Int
--   NfNe :: (NBE a) => Ne a -> Nf a
--   NLam ::
--     (NBE a, NBE b) =>
--     (Exp a -> Nf b) ->
--     Nf (a -> b)

-- embNe :: Ne a -> Exp a
-- embNe (NVar x) = Var x
-- embNe (NApp f e) = App (embNe f) (embNf e)

-- embNf :: Nf a -> Exp a
-- embNf (NInt n) = ConsN n
-- embNf (NLam f) = Lam $ \e -> embNf (f e)
-- embNf (NfNe e) = embNe e

-- test1 :: Exp Int
-- test1 = ConsN 1

-- test2 =
--   let f e = add (add e e) e
--       funExp = Lam f
--       applied = App funExp (ConsN 1)
--    in norm applied

-- add :: Exp Int -> Exp Int -> Exp Int
-- add (ConsN e1) (ConsN e2) = ConsN (e1 + e2)

-- quote :: (NBE a) => Host a -> Exp a
-- quote = embNf . reify

-- type Bindings = Map.Map String (Exp Int)
-- type Env = Map.Map String (Nf Int)

-- data Cxt = Cxt
--   { cxtSig :: Bindings
--   , cxtEnv :: Env
--   }

-- eval :: Cxt -> Exp Int -> Host Int
-- eval cxt v@(Var id) =
--   case Map.lookup id (cxtEnv cxt) of
--     Just (NfNe n) -> eval cxt $ embNe n
--     Just (NInt n) -> Right n
--     -- Just (Closure env _x e) -> eval (cxt{cxtEnv = env}) e
--     Nothing -> case Map.lookup id (cxtSig cxt) of
--       Just e -> eval (cxt{cxtEnv = Map.empty}) e
--       Nothing -> error ("unbound variable" ++ show id)
-- eval cxt (Lam f) = \(x :: Either (Exp a) (Host a)) -> case x of
--   Left e ->
--     let application = f e
--      in Left application
--   Right (v :: Host a) ->
--     let
--       v' = quote @a v
--      in
--       Right $ eval  $ f v'
-- eval cxt (App expF exp) =
--   let expF' = trace "evaluating expF'" $ eval expF
--       expr = trace "evaluating expr" $ eval exp
--       test = trace "evaluating application" $ expF' (Left exp)
--    in case test of
--         Left e -> eval e
--         Right v -> v
-- -- Primitives
-- eval cxt (ConsN n) = Right n
-- eval cxt (Add expr1 expr2) =
--   let expr1' = eval expr1
--       expr2' = eval expr2
--    in case (expr1', expr2') of
--         (Right n1, Right n2) -> Right $ n1 + n2
--         (Left e1, Right n2) -> Left $ Add e1 (ConsN n2) -- Left $ Add ( e1)  $ embNe n2  -- Left $ NAdd1 e1 n2
--         (Right n1, Left e2) -> Left $ Add (ConsN n1) e2 --  Left $ NAdd2 n1 e2
--         (Left e1, Left e2) -> Left $ Add e1 e2 -- undefined --  Left $ NAdd e1 e2

-- -- Type class for reify & reflect
-- class NBE obj where
--   type Host obj :: Type
--   reify :: Host obj -> Nf obj
--   reflect :: Ne obj -> Host obj

-- -- instance NBE Int where
-- --   type Host Int = Either (Ne Int) Int

-- --   -- This type already considers being suspended
-- --   reify :: Host Int -> Nf Int
-- --   reify (Left exp) =
-- --     let
-- --       exp' = embNe exp
-- --      in
-- --       NfNe exp
-- --   -- reify @Int exp'
-- --   reify (Right n) = NInt n

-- --   reflect :: Ne Int -> Host Int
-- --   reflect e = Left e

-- instance NBE Int where
--   type Host Int = Either (Exp Int) Int

--   -- This type already considers being suspended
--   reify :: Host Int -> Nf Int
--   reify (Left exp) =
--     let
--       exp' = eval exp
--      in
--       reify @Int exp'
--   reify (Right n) = NInt n
--   reflect :: Ne Int -> Host Int
--   reflect e = (eval $ trace "deadlock" $ embNe $ e)

-- -- read a NVar id :: Ne Int
-- --  convert it back to Var id :: Exp Int
-- --  eval (Var id), returns (NVar id) :: Ne Int deadloops

-- instance (NBE a, NBE b) => NBE (a -> b) where
--   type Host (a -> b) = Either (Exp a) (Host a) -> Either (Exp b) (Host b)

--   reify :: (NBE a, NBE b) => Host (a -> b) -> Nf (a -> b)
--   reify f = NLam $ \e ->
--     let e' = eval e -- This is to check if there is any Symbolic variables
--      in case f (Right e') of
--           Left expr -> reify $ eval expr
--           Right v -> reify v

--   reflect :: (NBE a, NBE b) => Ne (a -> b) -> Host (a -> b)
--   reflect expF = \eitherA ->
--     case eitherA of
--       Left expA ->
--         -- Left  $ App expF expA
--         let
--           exprA' = norm expA
--           expF' = embNe expF
--          in
--           trace "left" $ Left $ App expF' exprA'
--       Right hostA ->
--         let
--           reificationA = embNf $ reify @a hostA
--           expF' = eval $ embNe expF
--           result = expF' (Right hostA)
--          in
--           trace "right" $ result

-- norm :: (NBE obj) => Exp obj -> Exp obj
-- norm exp = quote $ eval exp

-- pExp :: Exp a -> StateT String IO Doc
-- pExp (Var id) = return $ pVar id
-- pExp (Lam f) = do
--   n <- get
--   put (n ++ "1")
--   body <- pExp (f (Var n))
--   return $ lbrace <> backslash <> pVar n <+> dot <+> body <> rbrace
-- pExp (App expF expA) =
--   (\f a -> f <+> char '@' <+> a)
--     <$> (pExp expF)
--     <*> (pExp expA)
-- pExp (ConsN n) = return $ int n
-- pExp (Add e1 e2) =
--   (\e1' e2' -> e1' <+> text "+" <+> e2')
--     <$> (pExp e1)
--     <*> (pExp e2)

-- pVar id = text "Var" <> lbrack <> text (show id) <> rbrack
-- dot = text "->"
-- backslash = char $ toEnum 92

-- see exp = evalStateT (pExp exp) "1" >>= putStrLn . show
