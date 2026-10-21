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
