--INF Obligatorisk oppgåve, H-2026
--Namn: Simon Toft og William Waly

data Ast = Var String
        | Val Int
        | Add Ast Ast
        | Sub Ast Ast
        | Mul Ast Ast
        | Div Ast Ast
        | App Ast Ast
        | Lambda String Ast
        deriving (Show, Eq)