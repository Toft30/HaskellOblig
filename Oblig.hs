--INF Obligatorisk oppgåve, H-2026
--Namn: Simon Toft og William Waly

import Data.Char (isSpace, isDigit, isLower, isAlpha)
import Control.Applicative (Alternative(..))

--Del 1
data Ast --her definere ej den rekursive datatypen Ast
    = Var String --bladnode som representere variablar
        | Val Int --bladnode som representere tall
        | Add Ast Ast
        | Sub Ast Ast
        | Mult Ast Ast
        | Div Ast Ast
        | App Ast Ast
        | Lambda String Ast
        deriving (Show, Eq) --gjer sann vi faktisk kan skrive ut, show gjer sann vi kan skrive ut, Eq gjer sann vi kan sjekke om to Ast e like

tokenise :: String -> [String] 
tokenise [] = [] 
tokenise (x:xs)
    |isSpace x = tokenise xs
    |isDigit x = let (num, rest) = span isDigit (x:xs) in num : tokenise rest
    |x == '|' = "|" : tokenise xs
    |x == '+' = "+" : tokenise xs
    |x == '-' && head xs == '>' = "->" : tokenise (tail xs)
    |x == '-' = "-" : tokenise xs
    |isLower x = let (var, rest) = span isLower (x:xs) in var : tokenise rest
    |otherwise = [x] : tokenise xs
    
--bruke parse fra veke 8
fyrsteElement :: [String] -> String -> Bool
fyrsteElement [] _ = False
fyrsteElement (x:xs) y = x == y

parse :: String -> Ast
parse s = case parseExpr (tokenise s) of
    (tree, []) -> tree
    (_, rest) -> error "ka i alle dage, uventa slutt på input"

parseExpr :: [String] -> (Ast, [String])
parseExpr tokens | fyrsteElement rest "+" = (Add tree tree2, rest2)
                 | fyrsteElement rest "-" = (Sub tree tree2, rest2)
                 | otherwise = (tree, rest)
    where
        (tree, rest) = parseExpr1 tokens
        (tree2, rest2) = parseExpr (tail rest)

parseExpr1 :: [String] -> (Ast, [String])
parseExpr1 tokens | fyrsteElement rest "*" = (Mult tree tree2, rest2)
                  | fyrsteElement rest "/" = (Div tree tree2, rest2)
                  | otherwise = (tree, rest)
    where
        (tree, rest) = parseValue tokens
        (tree2, rest2) = parseExpr1 (tail rest)

parseValue :: [String] -> (Ast, [String])
parseValue ("|" : var : "->" : rest) = 
    let (bodyTree, restBody) = parseExpr rest
    in (Lambda var bodyTree, restBody)
parseValue ("[" : rest) = 
    let (v1, rest1) = parseValue rest
        (v2, rest2) = parseValue rest1
    in case rest2 of 
        ("]" : restFinal) -> (App v1 v2, restFinal)
        _ -> error "mangla ein ']' til slutt"

--Del 2
--Uendeleg liste med typevariabelnamn: "t1", "t2", "t3", ...
typeVars :: [String]
typeVars = ["t" ++ show x| x <- [1..]]

--2.2 Dei moglege typeuttrykka: NumT, funksjonstype og typevariabel
data Type
    = NumT
    | Fun Type Type        --funksjonstype
    | TVar String          --typevariabel
    deriving Eq

--Show kan skrivast eksplisitt for å få pilar og parentesar, t.d. (NumT->t1)->t1
instance Show Type where
    show NumT = "NumT"
    show (TVar name) = name
    show (Fun a@(Fun _ _) b) = "(" ++ show a ++ ")->" ++ show b
    show (Fun a b) = show a ++ "->" ++ show b

--2.3 Forekjem typevariabelen med dette namnet i typeuttrykket?
occursIn :: String -> Type -> Bool
occursIn _ NumT = False
occursIn name (TVar other) = name == other
occursIn name (Fun a b) = occursIn name a || occursIn name b

type Eqn = (Type, Type)

unify :: [Eqn] -> Type -> Type
unify [] res = res
unify ((t0, t1) : eqns) res
    | t0 == t1 = unify eqns res
unify ((TVar x, t2) : eqns) res
    | occursIn x t2 = --error? 
    |otherwise = --på ett eller anna vis unify 

--del 3
type Context = [(Ast, Type)]

initialContext :: Context
initialContext = 
    [(Var "+", Fun NumT (Fun 
    NumT NumT))
    , (Var "-", Fun NumT (Fun 
    NumT NumT))
    , (Var "*", Fun NumT (Fun 
    NumT NumT))
    , (Var "/", Fun NumT (Fun 
    NumT NumT))]

generateEqns :: Context -> Ast -> String -> [String] -> ([Eqn], [String])
generateEqns ctx (Val _) varNamn rest = ([(TVar varNamn, NumT)], rest) 
generateEqns ctx (Var x) varNamn rest = ([(TVar varNamn, )])

