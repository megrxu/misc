import Untyped (env0, ext, lkup)

data Typ = TInt | Typ :> Typ
  deriving (Show, Eq)

infixr 9 :>

type VarName = String

data Term
  = V VarName
  | L VarName Typ Term
  | A Term Term
  | I Int
  | Term :+ Term
  | Fix Term
  | IFZ Term Term Term
  deriving (Show, Eq)

type TEnv = [(VarName, Typ)]

teval :: TEnv -> Term -> Typ
teval env (V x) = lkup env x
teval env (L x t e) = t :> teval (ext env (x, t)) e
teval env (A e1 e2) =
  let t1 = teval env e1
      t2 = teval env e2
   in case t1 of
        t1a :> t1r | t1a == t2 -> t1r
        t1a :> t1r -> error $ "Applying a function of arg type " ++ show t1a ++ " to argument of type " ++ show t2
        t1 -> error $ "Tring to apply a non-function: " ++ show t1
teval env (I n) = TInt
teval env (e1 :+ e2) =
  let t1 = teval env e1
      t2 = teval env e2
   in case (t1, t2) of
        (TInt, TInt) -> TInt
        ts -> error $ "Trying to add non-integers: " ++ show ts
teval env (IFZ e1 e2 e3) =
  let t1 = teval env e1
      t2 = teval env e2
      t3 = teval env e3
   in case t1 of
        TInt | t2 == t3 -> t2
        TInt ->
          error $
            unwords ["Branches of IFZ have different types:", show t2, "and", show t3]
        t -> error $ "Trying to compare a non-integer to 0: " ++ show t
teval env (Fix e) =
  let t = teval env e
   in case t of
        (ta1 :> tb1) :> (ta2 :> tb2) | ta1 == ta2 && tb1 == tb2 -> ta1 :> tb1
        t -> error $ "Inappropriate type in Fix: " ++ show t

(vx, vy) = (V "x", V "y")

term1 = L "x" TInt (IFZ vx (I 1) (vx :+ I 2))

test11 = teval env0 term1

test12 = teval env0 (term1 `A` I 2)

term3 = L "x" TInt (IFZ vx (I 1) vy)

test32 = teval env0 (term3 `A` I 0)

test33 = teval env0 (term3 `A` I 1)

tmul1 =
  L
    "x"
    TInt
    ( L
        "y"
        TInt
        ( IFZ
            vx
            (I 0)
            ((tmul1 `A` (vx :+ I (-1)) `A` vy) :+ vy)
        )
    )

testm1 = teval env0 (tmul1 `A` I (-1) `A` I (-1))

delta = L "y" (TInt :> TInt) (vy `A` vy)

-- termY = L "f" (delta `A` L "y" (L "x" (vf `A` (delta `A` vy) `A` vx)))

tmul =
  Fix
    ( L
        "self"
        (TInt :> TInt :> TInt)
        ( L
            "x"
            TInt
            ( L
                "y"
                TInt
                ( IFZ
                    vx
                    (I 0)
                    ((V "self" `A` (vx :+ I (-1)) `A` vy) :+ vy)
                )
            )
        )
    )

testm21 = teval env0 tmul

testm24 = teval env0 (tmul `A` I (-1) `A` I (-1))