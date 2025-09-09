module APL.AST
  (
    Exp(..),
    Error,
    VName
  )
where

data Exp
  = CstInt Integer
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | CstBool Bool
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  deriving (Eq, Show)

type Error = String
type VName = String