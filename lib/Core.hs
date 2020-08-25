-- | Module that provides the core types for the application.
module Core
    ( Sym (Sym)
    , Term (Var, Abs, App)
    , Result (Ok, Err)
    , fail
    , none
    , show
    ) where

import Control.Applicative (Applicative, (<*>), pure)
import Control.Monad (Monad, (>>=))
import Data.Bool (Bool (True, False), not)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor (Functor, fmap)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Text.Show (Show)

-- | Symbol type, a central construct for symbolic computation.
newtype Sym = Sym Text
    deriving (Eq, Show)

-- | Term type, a central construct in the untyped λ-calculus.
data Term
    = Var Sym       -- ^ Variable
    | Abs Sym Term  -- ^ Abstraction
    | App Term Term -- ^ Application
    deriving (Eq)

-- | Render a term as text.
show :: Term       -- ^ Term to render
     -> Text       -- ^ Result of rendering
show = show' True True

-- | Render a term as text.
show' :: Bool       -- ^ Whether this is a left edge
      -> Bool       -- ^ Whether this is a right edge
      -> Term       -- ^ Term to render
      -> Text       -- ^ Result of rendering
show' _ _ (Var (Sym x)) = x
show' _ r (Abs (Sym x) t) = paren' (not r) $ "λ" <> x <> "." <> (show' True r t)
show' l r (App p q) = paren' (not l) $ (show' l False p) <> " " <> (show' False r q)

paren' :: Bool      -- ^ Whether to parenthesize
       -> Text      -- ^ Input text
       -> Text      -- ^ Parenthesized text, if first arg is true
paren' y x = if y then "(" <> x <> ")" else x

-- | Result of an evaluation.
data Result a
    = Ok (Maybe a)  -- ^ Computation succeeded with a result
    | Err Text      -- ^ Computation encountered an error

-- | Construct an error result.
fail :: Text -> Result a
fail msg = Err msg

-- | Construct an empty but successful result.
none :: Result a
none = Ok Nothing

instance Functor Result where
    fmap :: (a -> b) -> Result a -> Result b
    fmap f (Ok x)  = Ok $ fmap f x
    fmap _ (Err e) = Err e

instance Applicative Result where
    pure :: a -> Result a
    pure x = Ok $ Just x

    (<*>) :: Result (a -> b) -> Result a -> Result b
    (<*>) (Ok (Just k)) (Ok (Just x)) = Ok $ Just $ k x
    (<*>) (Ok _) (Ok _) = Ok Nothing
    (<*>) (Err e) _ = Err e
    (<*>) _ (Err e) = Err e

instance Monad Result where
    (>>=) :: Result a -> (a -> Result b) -> Result b
    (>>=) (Ok (Just x)) f = f x
    (>>=) (Ok Nothing) _  = Ok Nothing
    (>>=) (Err e) _       = Err e
