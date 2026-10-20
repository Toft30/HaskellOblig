--INF Obligatorisk oppgåve, H-2026
--Namn: Simon Toft og William Waly

import Data.Char (isSpace)

--Del 1
data Ast --her definere ej den rekursive datatypen Ast
    = Var String --bladnode som representere variablar
        | Val Int --bladnode som representere tall
        | Add Ast Ast
        | Sub Ast Ast
        | Mul Ast Ast
        | Div Ast Ast
        | App Ast Ast
        | Lambda String Ast
        deriving (Show, Eq) --gjer sann vi faktisk kan skrive ut, show gjer sann vi kan skrive ut, Eq gjer sann vi kan sjekke om to Ast e like

tokenise :: String -> [String] 
tokenise [] = [] 
tokenise (x:xs)
    |isSpace x = tokenise xs
    |x == '|' = "|" : tokenise xs
    |x == 'a' = "a" : tokenise xs
    |x == '+' = "+" : tokenise xs

