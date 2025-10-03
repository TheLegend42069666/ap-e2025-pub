module APL.AST
  ( VName,
    Exp (..),
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

-- Atom ::= var
--        | int
--        | bool
--        | "(" Exp ")"

-- Exp ::= Atom
--       | Exp "+" Exp
--       | Exp "-" Exp
--       | Exp "*" Exp
--       | Exp "/" Exp

-- Exp0' ::=            (* empty *)
--         | "+" Atom Exp0'
--         | "-" Atom Exp0'
--         | "*" Atom Exp0'
--         | "/" Atom Exp0'