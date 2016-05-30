module Parser (Tree(..), parse) where
import Lexer


-- cteni double?
-- list of subnodes
data Tree = AtomNode String
          | ListNode [Tree]
          | ConsNode (Tree, Tree)
          | NumNode Double
          | ErrorNode String
          | LambdaNode Int
    deriving (Show, Eq)
    
    
parse :: [Token] -> Tree
parse toks = let (tree, toks') = term toks
             in
               if null toks' 
               then tree
               else error $ "Leftover tokens: " ++ show toks'

-- refactor factor?
term :: [Token] -> (Tree, [Token])
term toks = 
    case lookAhead toks of
        (TokNum x)     -> (NumNode x, accept toks)
        (TokAtom str)  -> if str == "nil" 
            then (ListNode [], accept toks)
            else (AtomNode str, accept toks)
        TokLParen      -> list (accept toks) []
        TokQuote -> let (tree', toks') = term (accept toks) in
            (ListNode [(AtomNode "quote"), tree'], toks')
        _ -> error $ "Parse error on token: " ++ show toks
      
      -- zacnu a neskoncim? asi ne
list :: [Token] -> [Tree] -> (Tree, [Token])
list toks treeList = 
    case lookAhead toks of
    -- refactorovat! append? proc vlastne
        (TokNum x)      -> list (accept toks) (treeList ++ [(NumNode x)])
        (TokAtom "nil") -> list (accept toks) (treeList ++ [(ListNode [])])
        (TokAtom str)   -> list (accept toks) (treeList ++ [(AtomNode str)])
        TokLParen       -> list (toks') (treeList ++ [innerList]) where
            (innerList, toks') = list (accept toks) []
        TokRParen       -> (ListNode treeList, accept toks)
        TokQuote        -> let (tree', toks') = term (accept toks) in
            list (toks') (treeList++[(ListNode [(AtomNode "quote"), tree'])]) 
--        TokQuote -> let (tree', toks') = term (accept toks) in
--            (ListNode [(AtomNode "quote"), tree'], toks')
--        null -> error $ "Right parenthesis is probably missing"
        _ -> error $ "Parse error on token: " ++ show toks

