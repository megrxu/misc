{-# LANGUAGE GADTs #-}

data Term t where
  V :: t -> Term t
  L :: (Term t1 -> Term t2) -> Term (t1 -> t2)
  A :: Term (t1 -> t2) -> Term t1 -> Term t2
  I :: Int -> Term Int
  (:+) :: Term Int -> Term Int -> Term Int
  IFZ :: Term Int -> Term t -> Term t -> Term t
  FIX :: Term ((a -> b) -> (a -> b)) -> Term (a -> b)

infixl 9 `A`

instance Show (Term t) where
  show (I n) = "I" ++ show n
  show (L f) = "<function>"

-- denotational semantics
evald :: Term t -> t
evald (V x) = x
evald (L f) = evald . f . V
evald (I n) = n
evald (A e1 e2) = evald e1 (evald e2)
evald (e1 :+ e2) = evald e1 + evald e2
evald (IFZ e1 e2 e3) = if evald e1 == 0 then evald e2 else evald e3
evald (FIX e) = fix (evald e) where fix f = f (fix f)

-- operational semantics
evalo :: Term t -> Term t
evalo (A e1 e2) =
  let v1 = evalo e1
      v2 = evalo e2
   in case v1 of
        L f -> evalo (f v2)
        _ -> error "Cannot happen" -- since haskell will type check that
evalo (e1 :+ e2) =
  let v1 = evalo e1
      v2 = evalo e2
   in case (v1, v2) of
        (I n1, I n2) -> I (n1 + n2)
        _ -> error "Cannot happen"
evalo (IFZ e1 e2 e3) =
  let v1 = evalo e1
   in case v1 of
        I 0 -> evalo e2
        I _ -> evalo e3
        _ -> error "Cannot happen"
evalo (FIX e) = evalo (e `A` FIX e)
evalo e = e

-- Tests
