module Main where
import System.Environment

import qualified Data.Map as M
import Lexer (tokenize)
import Parser (parse)
import Evaluator



main = do
--   args <- getArgs
-- typy?!
   loop (M.fromList [])

loop symTab = do
   str <- getLine
   if null str
   then
      return ()
   else
      let toks  = tokenize str
          tree  = parse toks
          Ev ev = evaluate tree symTab
      in
          case ev of
          Left msg -> do
              putStrLn $ "Error: " ++ msg
              loop symTab -- use old symTab
          Right (v, symTab') -> do
              print v
              loop symTab'
