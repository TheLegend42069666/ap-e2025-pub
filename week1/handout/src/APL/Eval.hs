module APL.Eval
  (
    Val(..),
    eval,
    envEmpty
  )
where

import APL.AST (Exp (..), Error, VName)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Env = [(VName, Val)]
envEmpty :: Env

envEmpty = []

envExtend :: VName -> Val -> Env -> Env

envExtend v_name val env = (v_name, val) : env

envLookup :: VName -> Env -> Maybe Val

envLookup v_name env = lookup v_name env

eval :: Env -> Exp -> Either Error Val

eval env (Var v) =
  case envLookup v env of
    Just x -> Right x
    Nothing -> Left $ "Unknown variable: " ++ v

eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2

eval env (CstInt x) = Right(ValInt x)

eval env (Add e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right(ValInt (x + y))

eval env (Sub e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right(ValInt (x - y))

eval env (Mul e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right(ValInt (x * y))

eval env (Div e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt _), Right (ValInt 0)) -> Left "division by zero"
    (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (x `div` y))

eval env (Pow e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt _), Right (ValInt y))|y < 0 -> Left "negative exponent"
    (Right (ValInt x), Right (ValInt y)) -> Right (ValInt (x ^ y))

eval env (CstBool x) = Right (ValBool x)

eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right (ValBool (x == y))
    (Right (ValBool x), Right (ValBool y)) -> Right (ValBool (x == y))
    (Right _, Right _) -> Left "Not same type"


eval env (If e1 e2 e3) =
  case eval env e1 of
    Left err -> Left err
    Right (ValBool True)  -> eval env e2
    Right (ValBool False) -> eval env e3
    Right _ -> Left "Not Boolean"
