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

data Type
    = NumT
    | Fun Type Type        --funksjonstype
    | TVar String          --typevariabel
    deriving Eq

--del 3
type Context = [(Ast, Type)]






