module IcfpLang (Expr(..), parseProgram, evaluate) where


charmap :: String
charmap = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

charToAscii :: Char -> Char
charToAscii c = let i = fromEnum c - 33 in
                if i < 0 then 'ä'
                else if i >= length charmap then 'ü'
                else charmap !! i

toAscii :: String -> String
toAscii = map charToAscii

data Expr = VBool !Bool
          | VInt Integer
          | VString String
          | Unary !Char Expr
          | Binary !Char Expr Expr
          | If Expr Expr Expr
          | Lambda Integer Expr
          | Var Integer
       deriving (Eq)

instance Show Expr where
  showsPrec _ (VBool False) = showChar 'F'
  showsPrec _ (VBool True) = showChar 'T'
  showsPrec _ (VInt n) = shows n
  showsPrec _ (VString s) = shows s . showChar ' ' . showParen True (shows (toAscii s))
  showsPrec _ (Unary op e) = showChar op . showChar ' ' . showParen True (shows e)
  showsPrec _ (Binary op a b) = showParen True (shows a) . showChar ' ' . showChar op
                              . showChar ' ' . showParen True (shows b)
  showsPrec _ (If a b c) = showString "if " . showParen True (shows a)
                         . showString " then " . showParen True (shows b)
                         . showString " else " . showParen True (shows c) . showString " fi"
  showsPrec _ (Lambda n e) = showChar '\\' . shows n . showString ". " . shows e
  showsPrec _ (Var n) = showChar 'v' . shows n



isValue :: Expr -> Bool
isValue (VBool _) = True
isValue (VInt _) = True
isValue (VString _) = True
isValue (Lambda _ _) = True
isValue (Var _) = True
isValue _ = False


parseInt :: String -> (Integer, String)
parseInt str = go str 0
  where
    go "" n = (n, "")
    go (' ':s) n = (n, s)
    go (c:s) n = go s (94 * n + fromIntegral (fromEnum c) - 33)

intToString :: Integer -> String
intToString num = if num == 0 then "!" else go "" num
  where
    go :: String -> Integer -> String
    go s n | n == 0 = s
           | otherwise = go (toEnum (fromIntegral (n `rem` 94 + 33)) : s) (n `quot` 94)


parse1 :: String -> (Expr, String)
parse1 (' ':p) = parse1 p
parse1 ('T':p) = (VBool True, p)
parse1 ('F':p) = (VBool False, p)
parse1 ('I':p) = let (n,p') = parseInt p in (VInt n, p')
parse1 ('S':p) = let (l,r) = break (' '==) p in (VString l, r)
parse1 ('U':op:' ':p) = let (arg, p') = parse1 p in (Unary op arg, p')
parse1 ('B':op:' ':p) = let (arg1, p') = parse1 p
                            (arg2, p'') = parse1 p'
                        in (Binary op arg1 arg2, p'')
parse1 ('?':p) = let (e1,p1) = parse1 p
                     (e2,p2) = parse1 p1
                     (e3,p3) = parse1 p2
                 in (If e1 e2 e3, p3)
parse1 ('L':p) = let (n,p') = parseInt p
                     (body,p'') = parse1 p'
                 in (Lambda n body, p'')
parse1 ('v':p) = let (n,p') = parseInt p in (Var n, p')
parse1 s = error ("invalid program: " ++ show s)

parseProgram :: String -> Expr
parseProgram = go
  where
    go s = let (expr, rest) = parse1 s in
           case dropWhile (' '==) rest of
             "" -> expr
             r -> error ("trailing garbage: " ++ show r)

evaluate :: Expr -> Expr
evaluate e | isValue e = e
           | otherwise = evaluate (eval e)


eval :: Expr -> Expr
eval (If e1 e2 e3) = case eval e1 of
                       VBool True -> e2
                       VBool False -> e3
                       ex -> error ("If: boolean expected, but got: " ++ show ex)
eval (Unary '-' e) = case eval e of
                       VInt n -> VInt (- n)
                       ex -> error ("U-: integer expected, but got: " ++ show ex)
eval (Unary '!' e) = case eval e of
                       VBool b -> VBool (not b)
                       ex -> error ("U!: boolean expected, but got: " ++ show ex)
eval (Unary '#' e) = case eval e of
                       VString s -> case parseInt s of
                                    (n,"") -> VInt n
                                    _ -> error "U#: invalid string"
                       ex -> error ("U#: string expected, but got: " ++ show ex)
eval (Unary '$' e) = case eval e of
                       VInt n -> VString (intToString n)
                       ex -> error ("U$: int expected, but got: " ++ show ex)
eval (Binary '$' t1 t2) = evalApp (eval t1) t2
eval (Binary op e1 e2) = apply2 op (eval e1) (eval e2)
eval e = e

evalApp :: Expr -> Expr -> Expr
evalApp (Lambda v body) e = subst v e body

subst :: Integer -> Expr -> Expr -> Expr
subst var val e@(Var v) = if var == v then val else e
subst var val (Unary op e) = Unary op (subst var val e)
subst var val (Binary op e1 e2) = Binary op (subst var val e1) (subst var val e2)
subst var val (If e1 e2 e3) = If (subst var val e1) (subst var val e2) (subst var val e3)
subst var val e@(Lambda v b) = if var == v then e else Lambda v (subst var val b)
subst _ _ e = e

apply2 :: Char -> Expr -> Expr -> Expr
apply2 '+' (VInt a) (VInt b) = VInt (a + b)
apply2 '-' (VInt a) (VInt b) = VInt (a - b)
apply2 '*' (VInt a) (VInt b) = VInt (a * b)
apply2 '/' (VInt a) (VInt b) = VInt (a `quot` b)
apply2 '%' (VInt a) (VInt b) = VInt (a `rem` b)
apply2 '<' (VInt a) (VInt b) = VBool (a < b)
apply2 '>' (VInt a) (VInt b) = VBool (a > b)
apply2 '=' a        b        = VBool (a == b)
apply2 '|' (VBool a) (VBool b) = VBool (a || b)
apply2 '&' (VBool a) (VBool b) = VBool (a && b)
apply2 '.' (VString a) (VString b) = VString (a ++ b)
apply2 'T' (VInt n) (VString s) = VString (take (fromInteger n) s)
apply2 'D' (VInt n) (VString s) = VString (drop (fromInteger n) s)
