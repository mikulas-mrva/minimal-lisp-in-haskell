-- TODO:
-- distribuce chyb -- overit (Ev typ?!)
-- proverit ten overlap (*) (+) (-) (/)
-- ?:
-- predavani globalniho scopu
-- -------------------
-- args
-- state monad?? vic monad?


module Evaluator (evaluate, Evaluator(..)) where


import Lexer
import Parser
import qualified Data.Map as M
import Data.Char

newtype Evaluator a = Ev (Either String a)


instance Applicative Evaluator where
    pure = Ev (Right v)
    f1 <*> f2 = f1 >>= \v1 -> f2 >>= (pure . v1)
    (Ev ev) >>= k =
        case (Ev ev) of
          Left msg -> Ev (Left msg)
          Right v -> k v

instance Monad Evaluator where
    --(Ev ev) >>= k =
    --    case ev of
    --      Left msg -> Ev (Left msg)
    --      Right v -> k v
    --return v = Ev (Right v)
    --(>>) = (*>)
    fail msg = Ev (Left msg)

type SymTab = M.Map String [Tree]


-- ERRORS
argNumError :: Tree -> SymTab -> (Tree, SymTab)
argNumError x symTab = (ErrorNode ("missing or extra expressions in "++(treeToStr x)), symTab)

argTypeError :: Tree -> SymTab -> (Tree, SymTab)
argTypeError x symTab = (ErrorNode ("invalid expression type "++(treeToStr x)), symTab)

defunArgTypeError :: Tree -> Tree
defunArgTypeError x = ErrorNode ("invalid parameter type "++(treeToStr x)++" (parameter name ought to be string)")

defunNameError :: String -> Tree
defunNameError x = ErrorNode ("invalid function name "++x++" (function name ought to start with an alpha char)")

defunArgNameError :: String -> Tree
defunArgNameError x = ErrorNode ("invalid parameter name "++x++" (parameter name ought to start with an alpha char)")

runTimeError :: Tree -> SymTab -> (Tree, SymTab)
runTimeError x symTab = (ErrorNode ("the following expression could not be run "++(treeToStr x)), symTab)

lambdaError :: Tree -> SymTab -> (Tree, SymTab)
lambdaError x symTab = (ErrorNode ("unassigned lambda found "++(treeToStr x)++" (a function proably needs more parameters)"), symTab)

divisionByZeroError :: Tree -> SymTab -> (Tree, SymTab)
divisionByZeroError x symTab = (ErrorNode ("division by zero in "++(treeToStr x)), symTab)


lispQuote :: Tree -> SymTab -> (Tree, SymTab)
lispQuote (ListNode (x:(ErrorNode e):xs)) symTab = (ErrorNode e, symTab)
lispQuote (ListNode [x, arg]) symTab = (arg, symTab)
lispQuote x symTab = argNumError x symTab


lispCar :: Tree -> SymTab -> (Tree, SymTab)
lispCar (ListNode (x:(ErrorNode e):xs)) symTab = (ErrorNode e, symTab)
lispCar (ListNode [x, (ConsNode (car, cdr))]) symTab = (car, symTab)
lispCar (ListNode [x, y]) symTab = case runTree y symTab of
    (ListNode (h:ts), symTab') -> (h, symTab')
    _ -> argTypeError (ListNode [x, y]) symTab
lispCar x symTab = argNumError x symTab


lispCdr :: Tree -> SymTab -> (Tree, SymTab)
lispCdr (ListNode (x:(ErrorNode e):xs)) symTab = (ErrorNode e, symTab)
lispCdr (ListNode [x, (ConsNode (car, cdr))]) symTab = (cdr, symTab)
lispCdr (ListNode [x, y]) symTab = case runTree y symTab of
    (ListNode (h:ts), symTab') -> (ListNode ts, symTab')
    _ -> argTypeError (ListNode [x, y]) symTab
lispCdr x symTab = argNumError x symTab


lispAtom :: Tree -> SymTab -> (Tree, SymTab)
lispAtom x symTab = let (x', symTab') = runTree x symTab in
    lispAtomInternal x' symTab'

lispAtomInternal :: Tree -> SymTab -> (Tree, SymTab)
lispAtomInternal (ListNode (x:(ErrorNode e):xs)) symTab = (ErrorNode e, symTab)
lispAtomInternal (ListNode [x, (AtomNode _)]) symTab = (AtomNode "t", symTab)
lispAtomInternal _ symTab = (ListNode [], symTab)


lispIf :: Tree -> SymTab -> (Tree, SymTab)
--lispIf (x:(ErrorNode e):xs) symTab = (ErrorNode e, symTab)
lispIf (ListNode [x, cond, yes, no]) symTab = case tree' of
    (ErrorNode e) -> (ErrorNode e, symTab)
    (ListNode []) -> runTree no symTab'
--    (BoolNode False) -> runTree no symTab' -- pryc
    _ -> runTree yes symTab
    where (tree', symTab') = runTree cond symTab
lispIf x symTab = argNumError x symTab


lispEq :: Tree -> SymTab -> (Tree, SymTab)
lispEq (ErrorNode e) symTab = (ErrorNode e, symTab)
lispEq (ListNode (x:(ErrorNode e):xs)) symTab = (ErrorNode e, symTab)
lispEq (ListNode [_, x, y])symTab = 
    let
        (tree1, symTab1) = runTree x symTab
        (tree2, symTab2) = runTree y symTab1
    in
        if tree1 == tree2
        then (AtomNode "t", symTab) 
        else (ListNode [], symTab) 
lispEq x symTab = argNumError x symTab


lispCons :: Tree -> SymTab -> (Tree, SymTab)
lispCons (ErrorNode e) symTab = (ErrorNode e, symTab)
lispCons (ListNode (x:(ErrorNode e):xs)) symTab = (ErrorNode e, symTab)
lispCons (ListNode [_, car, cdr]) symTab = (ConsNode (fst (runTree car symTab), fst (runTree cdr symTab)), symTab)
lispCons x symTab = argNumError x symTab


-- pozor na uvozovky
-- defun:name:lambdas:[body]
lispDefun :: Tree -> SymTab -> (Tree, SymTab)
lispDefun (ErrorNode e) symTab = (ErrorNode e, symTab)
lispDefun (ListNode (x:(AtomNode name):(ListNode lambdas):body)) symTab = let
        bodySubst = (substituteLambdas (pairLambdas lambdas) (ListNode body))
        check = checkLambdas lambdas 
    in 
        if check == (AtomNode "t")
            then case bodySubst of 
                (ListNode body') -> addSymbol name body' symTab
                _                -> (bodySubst, symTab)
            else (check, symTab)
lispDefun x symTab = argTypeError x symTab


checkLambdas :: [Tree] -> Tree
checkLambdas [] = (AtomNode "t")
checkLambdas x@((AtomNode str@(y:ys)):xs) -- neprazdny str
    | isAlpha $ head str = checkLambdas xs
    | otherwise = defunArgNameError str
checkLambdas x = defunArgTypeError (ListNode x)


-- 
pairLambdas :: [Tree] -> [(Tree, Tree)]
pairLambdas lambdas = map (\i -> ((LambdaNode i), lambdas!!(i-1))) [1..length lambdas]


-- proleze a vymeni prvni za druhe (tedy substituce lambdy za atomy nebo obracene)
substituteLambdas :: [(Tree, Tree)] -> Tree -> Tree
substituteLambdas lambdaPairs (ListNode []) = (ListNode [])
substituteLambdas lambdaPairs (ListNode listNode) = 
    (ListNode (map (substituteLambdas lambdaPairs) listNode))
substituteLambdas lambdaPairs (ConsNode (car, cdr)) = 
    (ConsNode (substituteLambdas lambdaPairs car, 
        substituteLambdas lambdaPairs cdr))
substituteLambdas lambdaPairs tree =
    let arr = filter (\x -> tree == (snd x)) lambdaPairs
    in case arr of
        [] -> tree
        _ -> fst $ head arr

   
        
lispPlus :: Tree -> SymTab -> (Tree, SymTab)
lispPlus (ListNode [x]) symTab = (NumNode 0, symTab)
lispPlus t@(ListNode [x, treeA, treeB]) symTab = let
        (resA, symTab1) = runTree treeA symTab
        (resB, symTab2) = runTree treeB symTab1
    in case (resA, resB) of
        ((NumNode a), (NumNode b)) -> (NumNode (a+b), symTab2)
        _ -> argTypeError t symTab
lispPlus x symTab = argNumError x symTab

lispMinus :: Tree -> SymTab -> (Tree, SymTab)
lispMinus t@(ListNode [x, treeA, treeB]) symTab = let
        (resA, symTab1) = runTree treeA symTab
        (resB, symTab2) = runTree treeB symTab1
    in case (resA, resB) of
        ((NumNode a), (NumNode b)) -> (NumNode (a-b), symTab2)
        _ -> argTypeError t symTab        
lispMinus x symTab = argNumError x symTab

lispTimes :: Tree -> SymTab -> (Tree, SymTab)
lispTimes (ListNode [x]) symTab = (NumNode 1, symTab)
lispTimes t@(ListNode [x, treeA, treeB]) symTab = let
        (resA, symTab1) = runTree treeA symTab
        (resB, symTab2) = runTree treeB symTab1
    in case (resA, resB) of
        ((NumNode a), (NumNode b)) -> (NumNode (a*b), symTab2)
        _ -> argTypeError t symTab        
lispTimes x symTab = argNumError x symTab

lispDiv :: Tree -> SymTab -> (Tree, SymTab)
lispDiv t@(ListNode [x, treeA, treeB]) symTab = let
        (resA, symTab1) = runTree treeA symTab
        (resB, symTab2) = runTree treeB symTab1
    in case (resA, resB) of
        (_, (NumNode 0)) -> divisionByZeroError t symTab
        ((NumNode a), (NumNode b)) -> (NumNode (a+b), symTab2)
        _ -> argTypeError t symTab        
lispDiv x symTab = argNumError x symTab


lispLt :: Tree -> SymTab -> (Tree, SymTab)
lispLt t@(ListNode [x, treeA, treeB]) symTab = let
        (resA, symTab1) = runTree treeA symTab
        (resB, symTab2) = runTree treeB symTab1
    in case (resA, resB) of
        ((NumNode a), (NumNode b)) -> if a < b then (AtomNode "t", symTab) else (ListNode [], symTab)
        _ -> argTypeError t symTab
lispLt x symTab = argNumError x symTab


lispFloor :: Tree -> SymTab -> (Tree, SymTab)
lispFloor (ListNode [x, a]) symTab = case runTree a symTab of
    ((NumNode n), symTab') -> (NumNode (fromIntegral (floor n)), symTab')
    (_, symTab')           -> argTypeError (ListNode [x, a]) symTab'
lispFloor x symTab = argNumError x symTab

lispError :: Tree -> SymTab -> (Tree, SymTab)
lispError x symTab = (ErrorNode (treeToStr x), symTab)

--builtIn = ["quote", "atom", "eq", "cons", "car", "cdr", "if", "defun", "+", "*", "/"]
runSymbol :: Tree -> SymTab -> (Tree, SymTab)
runSymbol (ErrorNode e) symTab = (ErrorNode e, symTab)
runSymbol (ListNode ((AtomNode name):params)) symTab = 
    let trees = lookUp name symTab
    in 
        case trees of
            [(ErrorNode e)] -> (ErrorNode e, symTab)
            _ -> (runTreeList (assignParams trees params) symTab, symTab) -- case sem? spojit s atomem
runSymbol (ListNode []) symTab = (ListNode [], symTab)
runSymbol (AtomNode "t") symTab = (AtomNode "t", symTab)
runSymbol x@_ symTab = runTimeError x symTab -- otestovat co delaj jiny interprety


revertPairs :: [(a, a)] -> [(a, a)]
revertPairs = map (\(a, b) -> (b, a))

assignParams :: [Tree] -> [Tree] -> [Tree]
assignParams trees params = 
    let 
        lambdaPairs = revertPairs $ pairLambdas params
    in
        map (substituteLambdas lambdaPairs) trees       


builtIn = [
    ("atom", lispAtom),
    ("car", lispCar),
    ("cdr", lispCdr),
    ("if", lispIf),
    ("cons", lispCons),
    ("defun", lispDefun),
    ("eq", lispEq),
    ("quote", lispQuote),
    ("+", lispPlus),
    ("-", lispMinus),
    ("*", lispTimes),
    ("/", lispDiv),
    ("<", lispLt),
    ("floor", lispFloor),
    ("error", lispError)]

isIn :: Eq a => a -> [a] -> Bool
isIn str lst = not . null $ filter (\x -> str == x) lst

runBuiltIn :: [(String, (Tree -> SymTab -> (Tree, SymTab)))] -> String -> Tree -> SymTab -> (Tree, SymTab) 
runBuiltIn builtIn str (ListNode (x:xs)) symTab = f (ListNode (x:xs)) symTab
    where f = (snd . head $ filter (\x -> (fst x) == str) builtIn) 
runBuiltIn builtIn str _ symTab = (ErrorNode "this makes no sense, sorry", symTab)


treeToStr :: Tree -> String
treeToStr (AtomNode str) = str
treeToStr (NumNode n) = show n
treeToStr (LambdaNode n) = "\\x" ++ (show n)
treeToStr (ConsNode (car, cdr)) = "("++(treeToStr car)++" . "++(treeToStr cdr)++")"
treeToStr (ListNode []) = "nil"
treeToStr (ListNode trees) = "("++(listToStr trees)++")"
treeToStr (ErrorNode e) = "ERROR: "++ e

listToStr :: [Tree] -> String
listToStr [] = ""
listToStr [x] = treeToStr x
listToStr (x:xs) = (treeToStr x)++" "++(listToStr xs)


runTree :: Tree -> SymTab -> (Tree, SymTab)                        
runTree (ErrorNode e) symTab = (ErrorNode e, symTab)
runTree (NumNode x) symTab = (NumNode x, symTab)
runTree (ListNode []) symTab = (ListNode [], symTab)
runTree (AtomNode "t") symTab = (AtomNode "t", symTab)
runTree (AtomNode "") symTab = (AtomNode "", symTab) -- nevim presne proc
runTree (AtomNode str) symTab
    | head str == '"' = (AtomNode str, symTab)
    | isIn str (map fst builtIn) = runBuiltIn builtIn str (AtomNode str) symTab  --zbavit se runBuiltIn
    | otherwise = runSymbol (AtomNode str) symTab
runTree tree@(LambdaNode n) symTab = lambdaError tree symTab
runTree tree@(ListNode (x:xs)) symTab =
    case x of 
        (AtomNode str) -> if isIn str (map fst builtIn) 
            then (runBuiltIn builtIn str tree symTab)
            else runSymbol tree symTab
        _ -> ((ErrorNode "invalid function name"), symTab)
runTree x symTab = runTimeError x symTab
        
runTreeList :: [Tree] -> SymTab -> Tree -- vracim posledni
runTreeList [a] symTab = fst $ runTree a symTab
runTreeList (x:xs) symTab = 
    let (tree2, symTab2) = runTree x symTab
    in runTreeList xs symTab2        
        
        
evaluate :: Tree -> SymTab -> Evaluator (String, SymTab)
evaluate tree symTab =
    return (treeToStr tree', symTab')
        where (tree', symTab') =  runTree tree symTab
    
        
-- case sensitive?
lookUp :: String -> SymTab -> [Tree]
lookUp str symTab = 
    case M.lookup str symTab of
      Just v  -> v
      Nothing -> [(ErrorNode ("Undefined variable " ++ str))]


addSymbol :: String -> [Tree] -> SymTab -> (Tree, SymTab)
addSymbol str val symTab =
    if isAlpha $ head str then 
        let symTab' = M.insert str val symTab
        in (AtomNode str, symTab')
    else (defunNameError str, symTab)

