module Main (main) where

import Control.Monad (void)
import Core (Term, Sym (Sym), Result (Ok, Err), show)
import Data.Bool (Bool (True, False))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (Just), fromJust, isJust)
import Data.Text (Text)
import Eval (eval, freeVars, isFree, parse)
import System.IO (IO)
import Test.HUnit (Test (TestCase, TestList))
import Test.HUnit.Lang (assertEqual)
import Test.HUnit.Text (runTestTT)

main :: IO ()
main = void $ runTestTT tsts

tsts :: Test
tsts = TestList $
    [ tstEval1
    , tstEval2
    , tstEval3
    , tstEval4
    , tstEval5
    , tstEval6
    , tstEval7
    , tstFreeVars1
    , tstFreeVars2
    , tstFreeVars3
    , tstFreeVars4
    , tstFreeVars5
    , tstIsFree1
    , tstIsFree2
    , tstIsFree3
    , tstIsFree4
    , tstIsFree5
    ]

formatResult :: Result Term -> Text
formatResult (Ok (Just t)) = show t
formatResult (Ok _) = "Empty result"
formatResult (Err e) = e

tstEval1 :: Test
tstEval1 = TestCase $ do
    let res = eval "(λx.x y) (λu.v u u)"
    assertEqual "Unexpected result" "v y y" (formatResult res)

tstEval2 :: Test
tstEval2 = TestCase $ do
    let res = eval "(λx y.y x) u v"
    assertEqual "Unexpected result" "v u" (formatResult res)

tstEval3 :: Test
tstEval3 = TestCase $ do
    let res = eval "(λx.x (x (y z)) x) (λu.u v)"
    assertEqual "Unexpected result" "y z v v λu.u v" (formatResult res)

tstEval4 :: Test
tstEval4 = TestCase $ do
    let res = eval "(λx.x x y) (λy.y z)"
    assertEqual "Unexpected result" "z z y" (formatResult res)

tstEval5 :: Test
tstEval5 = TestCase $ do
    let res = eval "(λx y.x y y) (λu.u y x)"
    assertEqual "Unexpected result" "λy'.y' y x y'" (formatResult res)

tstEval6 :: Test
tstEval6 = TestCase $ do
    let res = eval "(λy' y.y' y y) (λu.u y x)"
    assertEqual "Unexpected result" "λy''.y'' y x y''" (formatResult res)

tstEval7 :: Test
tstEval7 = TestCase $ do
    let res = eval "(λx y z.x z (y z)) ((λx y.y x) u) ((λx y.y x) v) w"
    assertEqual "Unexpected result" "w u (w v)" (formatResult res)

tstFreeVars1 :: Test
tstFreeVars1 = TestCase $ do
    let maybeTerm = parse "λw.x y"
    assertEqual "Invalid parse result" True (isJust maybeTerm)
    assertEqual "Unexpected free variables" (Sym <$> ["x" :: Text, "y"]) (freeVars (fromJust maybeTerm))

tstFreeVars2 :: Test
tstFreeVars2 = TestCase $ do
    let maybeTerm = parse "λx.x y"
    assertEqual "Invalid parse result" True (isJust maybeTerm)
    assertEqual "Unexpected free variables" (Sym <$> ["y" :: Text]) (freeVars (fromJust maybeTerm))

tstFreeVars3 :: Test
tstFreeVars3 = TestCase $ do
    let maybeTerm = parse "λx.x"
    assertEqual "Invalid parse result" True (isJust maybeTerm)
    assertEqual "Unexpected free variables" [] (freeVars (fromJust maybeTerm))

tstFreeVars4 :: Test
tstFreeVars4 = TestCase $ do
    let maybeTerm = parse "(λw.w x) (λy.w x y)"
    assertEqual "Invalid parse result" True (isJust maybeTerm)
    assertEqual "Unexpected free variables" (Sym <$> ["x" :: Text, "w"]) (freeVars (fromJust maybeTerm))

tstFreeVars5 :: Test
tstFreeVars5 = TestCase $ do
    let maybeTerm = parse "(λw.w y λy.y)"
    assertEqual "Invalid parse result" True (isJust maybeTerm)
    assertEqual "Unexpected free variables" (Sym <$> ["y" :: Text]) (freeVars (fromJust maybeTerm))

tstIsFree1 :: Test
tstIsFree1 = TestCase $ do
    let maybeTerm = parse "λw.x y"
    assertEqual "Invalid parse result" True (isJust maybeTerm)
    assertEqual "Invalid free variable" False (isFree (Sym "w") (fromJust maybeTerm))
    assertEqual "Invalid free variable" True  (isFree (Sym "x") (fromJust maybeTerm))
    assertEqual "Invalid free variable" True  (isFree (Sym "y") (fromJust maybeTerm))

tstIsFree2 :: Test
tstIsFree2 = TestCase $ do
    let maybeTerm = parse "λx.x y"
    assertEqual "Invalid parse result" True (isJust maybeTerm)
    assertEqual "Invalid free variable" False (isFree (Sym "x") (fromJust maybeTerm))
    assertEqual "Invalid free variable" True  (isFree (Sym "y") (fromJust maybeTerm))

tstIsFree3 :: Test
tstIsFree3 = TestCase $ do
    let maybeTerm = parse "λx.x"
    assertEqual "Invalid parse result" True (isJust maybeTerm)
    assertEqual "Invalid free variable" False (isFree (Sym "y") (fromJust maybeTerm))

tstIsFree4 :: Test
tstIsFree4 = TestCase $ do
    let maybeTerm = parse "(λw.w x) (λy.w x y)"
    assertEqual "Invalid parse result" True (isJust maybeTerm)
    assertEqual "Invalid free variable" True  (isFree (Sym "w") (fromJust maybeTerm))
    assertEqual "Invalid free variable" True  (isFree (Sym "x") (fromJust maybeTerm))
    assertEqual "Invalid free variable" False (isFree (Sym "y") (fromJust maybeTerm))

tstIsFree5 :: Test
tstIsFree5 = TestCase $ do
    let maybeTerm = parse "(λw.w y λy.y)"
    assertEqual "Invalid parse result" True (isJust maybeTerm)
    assertEqual "Invalid free variable" False (isFree (Sym "w") (fromJust maybeTerm))
    assertEqual "Invalid free variable" False (isFree (Sym "x") (fromJust maybeTerm))
    assertEqual "Invalid free variable" True  (isFree (Sym "y") (fromJust maybeTerm))
