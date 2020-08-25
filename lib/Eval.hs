-- | Module that provides the evaluator for the application.
module Eval
    ( eval
    , freeVars
    , isFree
    , parse
    ) where

import Control.Applicative ((<*), pure)
import Core (Result, Term (Var, Abs, App), Sym (Sym), fail, none)
import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.Bool (Bool (False), (||), otherwise)
import Data.Either.Combinators (rightToMaybe)
import Data.Eq ((==))
import Data.Function ((.), ($))
import Data.List ((++), elem, nub)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Semigroup ((<>))
import Data.Text (Text, length, stripStart)
import Parser (termParser)

-- | Evaluate input text as a λ-term, convert the resultant
--   abstract syntax tree into its β-normal or minimal form
--   and return it. If the input cannot be parsed, an error
--   is returned. If the λ-term diverges, evaluation will
--   continue indefinitely.
eval :: Text                -- ^ Input text
     -> Result Term         -- ^ Result with constructed term
eval inp = do
    if length txt == 0
       then none
       else case parse txt of
           Nothing  -> fail "Invalid expression!"
           (Just t) -> reduce (freeVars t) t
  where
    txt = stripStart inp

-- | Parse input text as a λ-term.
parse :: Text               -- ^ Text to parse (cannot be empty)
      -> Maybe Term         -- ^ Result or error
parse = rightToMaybe . parseOnly (termParser <* endOfInput)

-- | Extract the list of symbols that are free variables.
freeVars :: Term            -- ^ Term to examine
         -> [Sym]           -- ^ Free variables
freeVars = nub . freeVars' [] []

freeVars' :: [Sym]          -- ^ Variables already bound
          -> [Sym]          -- ^ Free variables found so far
          -> Term           -- ^ Term to consider
          -> [Sym]          -- ^ Free variables
freeVars' l c (Var x) = if elem x l then c else (x:c)
freeVars' l c (Abs x t) = freeVars' (x:l) c t
freeVars' l c (App p q) = (freeVars' l c p) ++ (freeVars' l c q)

-- | Determines if the given variable is free in the given term.
isFree :: Sym               -- ^ Symbol to look for
       -> Term              -- ^ Term to look within
       -> Bool              -- ^ True if it is a free variable
isFree s (Var v)   = s == v
isFree s (App p q) = isFree s p || isFree s q
isFree s (Abs x r)
  | s == x    = False
  | otherwise = isFree s r

-- | Reduce the term to its β-normal or minimal form.
reduce :: [Sym]             -- ^ Free variables in top-level binding
       -> Term              -- ^ Term to reduce
       -> Result Term       -- ^ Result of reduction
reduce f t = do
    t' <- reduceOnce [] f t
    if t == t'
       then pure t
       else reduce f t'

-- | Reduce the term *by one step* towards its β-normal or
--   minimal form.
reduceOnce :: [Sym]         -- ^ Variables already bound
           -> [Sym]         -- ^ Free variables in top-level binding
           -> Term          -- ^ Term to reduce
           -> Result Term   -- ^ Result of reduction by a step
reduceOnce _ _ (Var x) = pure $ Var x
reduceOnce b f (App (Abs s t) q) = do
    pure $ subs (s:b) f s q t
reduceOnce b f (App p q) = do
    p' <- reduceOnce b f p
    q' <- reduceOnce b f q
    pure $ App p' q'
reduceOnce b f (Abs s t) = do
    t' <- reduceOnce (s:b) f t
    pure $ Abs s t'

-- | Substitute all occurrences of a symbol in a term.
subs :: [Sym]               -- ^ Variables already bound
     -> [Sym]               -- ^ Free variables in top-level binding
     -> Sym                 -- ^ Symbol to look for
     -> Term                -- ^ Term to substitute with
     -> Term                -- ^ Term to substitute within
     -> Term                -- ^ Result of the subtitution
subs _ _ x t (Var s)
  | x == s    = t
  | otherwise = Var s
subs b f x t (App p q) = App (subs b f x t p) (subs b f x t q)
subs b f x t (Abs s r)
  | x == s = Abs s r        -- s hides x
  | otherwise = Abs s' (subs (s':b) f x t r')
  where
    v = freshVar b f s
    (s', r') = if isFree s t
                  then (v, subs (v:b) f s (Var v) r)
                  else (s, r)

-- | Provide a fresh variable available for substitution.
--
--   The algorithm for generating a fresh variable is this: we append
--   an apostrophe (') to the end of the variable, and check if it is
--   an existing free variable in the global context. If it isn't, we
--   are done; if it is, then we try again.
freshVar :: [Sym]           -- ^ Variables already bound
         -> [Sym]           -- ^ Free variables in top-level binding
         -> Sym             -- ^ Seed to pick deterministic value
         -> Sym             -- ^ Fresh symbol
freshVar b f (Sym x) = if elem n f || elem n b
                          then freshVar b f n
                          else n
  where
    n = Sym $ x <> "'"
