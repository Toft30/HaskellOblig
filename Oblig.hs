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
        | Mul Ast Ast
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
    |isAlpha x = let (var, rest) = span isAlpha (x:xs) in var : tokenise rest
    |otherwise = [x] : tokenise xs
    
newtype Parser a = Parse (String -> [(a, String)])

natural :: Parser Int
natural = Parse (\inp ->
    let inp' = dropWhile isSpace inp --hoppe over myllarom
        (num, rest) = span isDigit inp'
    in if null num
        then []
        else [(read num, dropWhile isSpace rest)])

parseHelper :: Parser a -> String -> [(a, String)]
parseHelper (Parse p) inp = p inp

symbol :: String -> Parser String
symbol xs = Parse (\inp ->
  let inp' = dropWhile isSpace inp
  in if take (length xs) inp' == xs
       then [(xs, dropWhile isSpace (drop (length xs) inp'))]
       else [])

identifier :: Parser String
identifier = Parse (\inp ->
  let inp' = dropWhile isSpace inp
      (var, rest) = span isAlpha inp'
  in if null var
       then []
       else [(var, dropWhile isSpace rest)])

instance Functor Parser where
    fmap f p = Parse (\inp -> [(f v, out) | (v, out) <- parseHelper p inp])

instance Applicative Parser where
    pure v = Parse (\inp -> [(v, inp)])
    pf <*> px = Parse (\inp -> [(f v, out2) | (f, out1) <- parseHelper pf inp,
                                             (v, out2) <- parseHelper px out1])

instance Monad Parser where
    p >>= f = Parse (\inp -> concat [parseHelper (f v) out | (v, out) <- parseHelper p inp])

instance Alternative Parser where
    empty = Parse (\_ -> [])
    p <|> q = Parse (\inp -> case parseHelper p inp of
                               [] -> parseHelper q inp
                               r  -> r)

parse :: String -> Ast
parse str = case parseHelper parseExpr str of
  [(tree, "")] -> tree
  [(_, rest)]  -> error ("Parseren stoppa før slutt, ulese: " ++ rest)
  []           -> error "Syntaksfeil: Kunne ikkje parse uttrykket"

parseExpr :: Parser Ast
parseExpr =
        (do e1 <- parseExpr1
            symbol "+"
            e2 <- parseExpr
            return (Add e1 e2))
    <|> (do e1 <- parseExpr1
            symbol "-"
            e2 <- parseExpr
            return (Sub e1 e2))
    <|> parseExpr1

parseExpr1 :: Parser Ast
parseExpr1 = 
        (do v <- parseValue
            symbol "*"
            e1 <- parseExpr1
            return (Mul v e1))
    <|> (do v <- parseValue
            symbol "/"
            e1 <- parseExpr1
            return (Div v e1))
    <|> parseValue

parseValue :: Parser Ast
parseValue = 
    (do n <- natural
        return (Val n))
    <|> (do symbol "|"
            x <- identifier
            symbol "->"
            e <- parseExpr
            return (Lambda x e))
    <|> (do symbol "["
            v1 <- parseValue
            v2 <- parseValue
            symbol "]"
            return (App v1 v2))
    <|> (do symbol "("
            e <- parseExpr
            symbol ")"
            return e)




