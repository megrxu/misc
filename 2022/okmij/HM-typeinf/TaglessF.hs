{-# LANGUAGE TupleSections #-}

class Symantics repr where
  l :: (repr t1 -> repr t2) -> repr (t1 -> t2)
  a :: repr (t1 -> t2) -> repr t1 -> repr t2
  i :: Int -> repr Int
  (+:) :: repr Int -> repr Int -> repr Int
  ifz :: repr Int -> repr t -> repr t -> repr t
  fix :: repr ((t1 -> t2) -> (t1 -> t2)) -> repr (t1 -> t2)

type VarCount = Int

newtype S t = S (VarCount -> (String, VarCount))

evals (S t) = t

-- showing
instance Symantics S where
  l f = S $ \c0 ->
    let vname = "v" ++ show c0
        c1 = succ c0
        (s, c2) = evals (f (S (vname,))) c1
     in ("(\\" ++ vname ++ "-> " ++ s ++ ")", c2)
  a e1 e2 = S $ \c0 ->
    let (s1, c1) = evals e1 c0
        (s2, c2) = evals e2 c1
     in ("(" ++ s1 ++ " " ++ s2 ++ ")", c2)
  i n = S (show n,)
  e1 +: e2 = S $ \c0 ->
    let (s1, c1) = evals e1 c0
        (s2, c2) = evals e2 c1
     in ("(" ++ s1 ++ " + " ++ s2 ++ ")", c2)
  ifz e1 e2 e3 = S $ \c0 ->
    let (s1, c1) = evals e1 c0
        (s2, c2) = evals e2 c1
        (s3, c3) = evals e3 c2
     in ("(ifz " ++ s1 ++ " " ++ s2 ++ " " ++ s3 ++ ")", c3)
  fix e = S $ \c0 ->
    let (s1, c1) = evals e c0
     in ("(fix " ++ s1 ++ ")", c1)

tshow t = fst $ evals t 0

-- denotational semantics
newtype D t = D t

evald :: D t -> t
evald (D t) = t

instance Symantics D where
  l f = D $ \x -> evald (f (D x))
  a e1 e2 = D $ evald e1 (evald e2)
  i n = D n
  e1 +: e2 = D $ evald e1 + evald e2
  ifz e1 e2 e3 = D $ if evald e1 == 0 then evald e2 else evald e3
  fix e = D $ hfix (evald e) where hfix f = f (hfix f)

