module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), eval, envEmpty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [testCase "evalInt" $
      eval envEmpty (CstInt 2) @?= Right(ValInt 2),
    --
    testCase "Add" $
      eval envEmpty (Add (CstInt 2) (CstInt 3)) @?= Right(ValInt 5),
    --
    testCase "Sub" $
      eval envEmpty (Sub (CstInt 2) (CstInt 3)) @?= Right(ValInt (-1)),
    --
    testCase "Mul" $
      eval envEmpty (Mul (CstInt 2) (CstInt 3)) @?= Right(ValInt 6),
    --
    testCase "Div" $
      eval envEmpty (Div (CstInt 6) (CstInt 2)) @?= Right(ValInt 3),
    --
    testCase "Zero_div" $
      eval envEmpty (Div (CstInt 6) (CstInt 0)) @?= Left "division by zero",
    --
    testCase "Pow" $
      eval envEmpty (Pow (CstInt 2) (CstInt 3)) @?= Right(ValInt 8),
    --
    testCase "Negative_exponent_Pow" $
      eval envEmpty (Pow (CstInt 2) (CstInt (-3))) @?= Left "negative exponent",
    --
    testCase "evalBool" $
      eval envEmpty (CstBool True) @?= Right(ValBool True),
    --
    testCase "Equal" $
      eval envEmpty (Eql (CstBool True) (CstBool True)) @?= Right (ValBool True),
    --
    testCase "Equal2" $
      eval envEmpty (Eql (CstInt 2) (CstInt 1)) @?= Right (ValBool False),
    --
    testCase "Equal3" $
      eval envEmpty (Eql (CstInt 2) (CstBool True)) @?= Left "Not same type",
    --
    testCase "If" $
      eval envEmpty (If (CstBool True) (CstInt 2) (CstInt 1)) @?= Right (ValInt 2),
    --
    testCase "If2" $
      eval envEmpty (If (CstBool (CstInt 2 == CstInt 2)) (CstInt 2) (CstInt 1)) @?= Right (ValInt 2)
    ]
