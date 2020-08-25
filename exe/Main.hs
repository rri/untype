-- | Main module for the command-line application.
module Main (main) where

import Control.Monad ((>>), return)
import Core (Result (Ok, Err), show)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Semigroup ((<>))
import Data.Text (Text, lines, replicate, strip, unlines)
import Data.Text.IO (getLine, putStr, putStrLn)
import Eval (eval)
import System.IO (IO, hFlush, stdout)

-- | Main entry-point for the command-line application.
main :: IO ()
main = do
    putStrLn edge
    putStrLn "↪ type a λ-expression to evaluate"
    putStrLn "↪ type Ctrl-C to exit, or kill a computation"
    putStrLn edge
    repl
  where
    edge = replicate 57 "-"

-- | Execute a read-eval-print-loop to accept and evaluate
--   input as a λ-expression and print its β-normal form.
--
--   Not every expression has a β-normal form. In such cases,
--   the evaluated result may either diverge (execute forever
--   and need to be terminated with a Ctrl-C) or be a minimal
--   (but not normal) form.
repl :: IO ()
repl = do
    i <- prompt >> accept
    case eval i of
        Ok res -> case res of
            Nothing -> return ()
            Just o  -> emit "↪" (show o)
        Err e  -> emit "✗" e
    repl
  where
    prompt = putStr ": " >> hFlush stdout
    accept = getLine

-- | Emit lines to the console, decorated appropriately.
emit :: Text        -- ^ Prefix
     -> Text        -- ^ Text to emit
     -> IO ()
emit prefix = putStrLn . strip . unlines . (((prefix <> " ") <>) <$>) . lines
