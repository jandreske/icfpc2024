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
          | VInt !Integer
          | VString !String
          | Unary !Char Expr
          | Binary !Char Expr Expr
          | If !Expr Expr Expr
          | Lambda !Int Expr
          | Var !Int
       deriving (Eq)

precedence :: Char -> Int
precedence '+' = 6
precedence '-' = 6
precedence '*' = 7
precedence '/' = 7
precedence '%' = 7
precedence '.' = 8
precedence '$' = 0
precedence _ = 4

instance Show Expr where
  showsPrec p expr =
    case expr of
      VBool False -> showString "False"
      VBool True  -> showString "True"
      VInt n      -> shows n
      VString s   -> shows (toAscii s)
      Var n       -> showChar 'v' . shows n
      Unary op e  -> showParen (p >= 11) $ showChar op . showsPrec 11 e
      Binary op a b -> let pr = precedence op in 
                       showParen (p >= pr) $ showsPrec pr a . showChar ' ' . showChar op . showChar ' ' . showsPrec pr b
      If a b c    ->  showString "if " . shows a . showString " then " . shows b
                         . showString " else " . shows c . showString " fi"
      Lambda v e -> showParen (p >= 2) $ showChar '\\' . shows v . showString " -> " . showsPrec 2 e


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
           | otherwise = go (toEnum (fromIntegral (n `rem` 94) + 33) : s) (n `quot` 94)


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
                 in (Lambda (fromInteger n) body, p'')
parse1 ('v':p) = let (n,p') = parseInt p in (Var (fromInteger n), p')
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
eval (If e1 e2 e3) = case evaluate e1 of
                       VBool True -> e2
                       VBool False -> e3
                       ex -> error ("If: boolean expected, but got: " ++ show ex)
eval (Unary '-' e) = case evaluate e of
                       VInt n -> VInt (- n)
                       ex -> error ("U-: integer expected, but got: " ++ show ex)
eval (Unary '!' e) = case evaluate e of
                       VBool b -> VBool (not b)
                       ex -> error ("U!: boolean expected, but got: " ++ show ex)
eval (Unary '#' e) = case evaluate e of
                       VString s -> case parseInt s of
                                    (n,"") -> VInt n
                                    _ -> error "U#: invalid string"
                       ex -> error ("U#: string expected, but got: " ++ show ex)
eval (Unary '$' e) = case evaluate e of
                       VInt n -> VString (intToString n)
                       ex -> error ("U$: int expected, but got: " ++ show ex)
eval (Binary '$' t1 t2) = evalApp (evaluate t1) t2
eval (Binary op e1 e2) = apply2 op (evaluate e1) (evaluate e2)

evalApp :: Expr -> Expr -> Expr
evalApp (Lambda v body) e = subst v e body
evalApp e1 _ = error ("B$: lambda expected, but got: " ++ show e1)

subst :: Int -> Expr -> Expr -> Expr
subst var val e@(Var v) = if var == v then val else e
subst var val (Unary op e) = Unary op (subst var val e)
subst var val (Binary op e1 e2) = Binary op (subst var val e1) (subst var val e2)
subst var val (If e1 e2 e3) = If (subst var val e1) (subst var val e2) (subst var val e3)
subst var val e@(Lambda v b) = if var == v then e
                               else let (v',b') = renameIfFree v b val in
                                    Lambda v' (subst var val b')
subst _ _ e = e

renameIfFree :: Int -> Expr -> Expr -> (Int, Expr)
renameIfFree v body expr | v `freeIn` expr = let v' = maxVar expr + 1 in (v', rename v v' body)
                         | otherwise = (v, body)

freeIn :: Int -> Expr -> Bool
v `freeIn` e = case e of
                 Var n -> v == n
                 Lambda n b -> v /= n && v `freeIn` b
                 Unary _ e1 -> v `freeIn` e1
                 Binary _ a b -> v `freeIn` a || v `freeIn` b
                 If a b c -> v `freeIn` a || v `freeIn` b || v `freeIn` c
                 _ -> False

maxVar :: Expr -> Int
maxVar = go (-1)
  where
    go n e = case e of
               Var v -> max v n
               Lambda v b -> go (max n v) b
               Unary _ a -> go n a
               Binary _ a b -> go (go n a) b
               If a b c -> go (go (go n a) b) c
               _ -> n               

rename :: Int -> Int -> Expr -> Expr
rename v v' e = case e of
                  Var n -> if n == v then Var v' else e
                  Lambda n b | n == v -> Lambda v' (rename v v' b)
                             | otherwise -> Lambda v (rename v v' b)
                  Unary op a -> Unary op (rename v v' a)
                  Binary op a b -> Binary op (rename v v' a) (rename v v' b)
                  If a b c -> If (rename v v' a) (rename v v' b) (rename v v' c)
                  _ -> e

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
