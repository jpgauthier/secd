--  SECD abstract machine

import Test.HUnit

data Primitive = Succ | IsZero

data Term = Var String | Abs String Term | App Term Term
          | Prim Primitive | INT Int

instance Show Term where
  show (Var v) = v
  show (Abs h b) = "(\\" ++ h ++ ".(" ++ show b ++ "))"
  show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
  show (Prim Succ) = "Succ"
  show (Prim IsZero) = "IsZero"
  show (INT i) = show i

applyPrim :: Primitive -> Term -> Term
applyPrim Succ (INT n) = INT (n+1)
applyPrim IsZero (INT n)
  | n == 0 = Abs "x" (Abs "y" (Var "x")) -- true
  | otherwise = Abs "x" (Abs "y" (Var "y")) -- false

data SContents = S Term | SClosure (String, Term, [EContents])
data EContents = E (String, SContents)
data CContents = C Term | CApply
data DContents = D ([SContents], [EContents], [CContents])
data SECDConfig = SECD ([SContents], [EContents], [CContents], [DContents])

instance Show SContents where
    show (S t) = show t
    show (SClosure (s, t, e)) = "<" ++ s ++ ", " ++ show t ++ ", " ++ show e ++ ">"

instance Show EContents where
    show (E (s, t)) = "(" ++ s ++ ", " ++ show t ++ ")"

instance Show CContents where
    show (C t) = show t
    show CApply = "@"

instance Show DContents where
    show (D (s, e, c)) = "<" ++ show s ++ ", " ++ show e ++ ", " ++ show c ++ ">"

instance Show SECDConfig where
    show (SECD (s, e, c, d)) =
        "S = " ++ show s ++ "\n" ++
        "E = " ++ show e ++ "\n" ++
        "C = " ++ show c ++ "\n" ++
        "D = " ++ show d ++ ""

lookUp :: String -> [EContents] -> SContents
lookUp s (E(x, t):xs)
  | x == s = t
  | otherwise = lookUp s xs

secdOneStep :: SECDConfig -> SECDConfig
secdOneStep (SECD(s, e, C (Var x) : c', d)) = SECD((lookUp x e) : s, e, c', d)
secdOneStep (SECD(s, e, C (Abs h b) : c', d)) = SECD(SClosure(h, b, e) : s, e, c', d)
secdOneStep (SECD(s, e, C (App x y) : c', d)) = SECD(s, e, C y : C x : CApply : c', d)
secdOneStep (SECD(s, e, C (Prim x) : c', d)) = SECD(S (Prim x) : s, e, c', d)
secdOneStep (SECD(s, e, C (INT x) : c', d)) = SECD(S (INT x) : s, e, c', d)
secdOneStep (SECD(S (Prim x) : S n : s', e, CApply : c', d)) = SECD(s', e, C (applyPrim x n) : c', d)
secdOneStep (SECD(SClosure(x, m, e1) : n : s', e, CApply : c', d)) = SECD([], E(x, n) : e1, [C m], D(s', e, c') : d )
secdOneStep (SECD([m], _, [], D(s', e', c') : d')) = SECD(m : s', e', c', d')
secdOneStep (SECD([m], e, [], [])) = SECD([m], e, [], [])

sub :: Term -> EContents -> Term
sub (Var x) (E(y, s))
  | x == y = closureToTerm s
  | otherwise = Var x
sub (Abs h b) (E(x, s))
  | h == x = Abs h b
  | otherwise = Abs x (sub b (E(x, s)))
sub (App x y) e = App (sub x e) (sub y e)

closureToTerm :: SContents -> Term
closureToTerm (S t) = t
closureToTerm (SClosure(s, t, e)) = foldl sub(Abs s t) e

runSECD :: SECDConfig -> SECDConfig
runSECD (SECD (s, e, [], [])) = SECD(s, e, [], [])
runSECD (SECD (s, e, c, d)) = runSECD(secdOneStep (SECD (s, e, c, d)))

reduce  :: Term -> Term
reduce t = closureToTerm (extract (runSECD (SECD ([], [], [C t], []))))
  where extract (SECD ([m], _, _, _)) = m


main = runTestTT $ TestList [ "t1"   ~: ("(\\x.((\\y.(x))))") ~=? (show (reduce (App (Prim IsZero) (INT 0)))),
                              "t2"   ~: ("1") ~=? (show (reduce (App (Prim Succ) (INT 0)))),
                              "t3"   ~: ("(\\x.(x))") ~=? (show (reduce (Abs "x" (Var "x")))),
                              "t4"   ~: ("0") ~=? (show (reduce (App (Abs "x" (Var "x")) (INT 0)))),
                              "t5"   ~: ("(\\x.((\\y.(x))))") ~=? (show (reduce (App (Prim IsZero) (App (Prim Succ) (App (Abs "z" (Var "z")) (INT (-1))))))),
                              "t6"   ~: ("(\\x.(x))") ~=? (show (reduce (App (Abs "z" (App (Var "z") (Var "z"))) (Abs "x" (Var "x")))))
                            ]
