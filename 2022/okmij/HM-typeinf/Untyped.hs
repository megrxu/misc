module Untyped where

import Data.Maybe (fromMaybe)

type VarName = String

data Term
  = V VarName
  | L VarName Term
  | A Term Term
  | I Int
  | Term :+ Term
  | IFZ Term Term Term
  deriving (Show, Eq)

infixl 9 `A`

data Value = VI Int | VC (Value -> Value)

instance Show Value where
  show (VI n) = "VI " ++ show n
  show (VC _) = "<function>"

type Env = [(VarName, Value)]

env0 = []

lkup env x = fromMaybe err $ lookup x env
  where
    err = error $ "Unbound variable: " ++ x

ext env xt = xt : env

eval :: Env -> Term -> Value
eval env (V x) = lkup env x
eval env (L x e) = VC (\v -> eval (ext env (x, v)) e)
eval env (A e1 e2) =
  let v1 = eval env e1
      v2 = eval env e2
   in case v1 of
        VC f -> f v2
        v -> error $ "Application of a non-function: " ++ show v
eval env (I n) = VI n
eval env (e1 :+ e2) =
  let v1 = eval env e1
      v2 = eval env e2
   in case (v1, v2) of
        (VI n1, VI n2) -> VI (n1 + n2)
        _ -> error $ "Non-numeric operands to +: " ++ show (v1, v2)
eval env (IFZ e1 e2 e3) =
  let v1 = eval env e1
   in case v1 of
        VI 0 -> eval env e2
        VI _ -> eval env e3
        v -> error $ "Non-numeric operands to ifz: " ++ show v

test00 = eval env0 (L "x" (IFZ (V "x") (I 1) (V "x" :+ I 2)) `A` I 10)

(vx, vy) = (V "x", V "y")

term1 = L "x" (IFZ vx (I 1) (vx :+ I 2))

test11 = eval env0 term1

test12 = eval env0 (term1 `A` I 2)

term3 = L "x" (IFZ vx (I 1) vy)

test32 = eval env0 (term3 `A` I 0)

test33 = eval env0 (term3 `A` I 1)

tmul1 =
  L
    "x"
    ( L
        "y"
        ( IFZ
            vx
            (I 0)
            ((tmul1 `A` (vx :+ I (-1)) `A` vy) :+ vy)
        )
    )

testm1 = eval env0 (tmul1 `A` I (-1) `A` I (-1))

termY = L "f" (delta `A` L "y" (L "x" (vf `A` (delta `A` vy) `A` vx)))
  where
    (vf, delta) = (V "f", L "y" (vy `A` vy))

tmul =
  termY
    `A` L
      "self"
      ( L
          "x"
          ( L
              "y"
              ( IFZ
                  vx
                  (I 0)
                  ((V "self" `A` (vx :+ I (-1)) `A` vy) :+ vy)
              )
          )
      )

testm2 = eval env0 (tmul `A` I (-1) `A` I (-1))

termid = L "x" vx

termlet =
  let c2 = L "f" (L "x" (V "f" `A` (V "f" `A` vx)))
      inc = L "x" (vx :+ I 1)
      compose =
        L
          "f"
          ( L
              "g"
              ( L
                  "x"
                  (V "f" `A` (V "g" `A` vx))
              )
          )
   in c2 `A` (compose `A` inc `A` inc) `A` I 10
        :+ ((c2 `A` (compose `A` inc) `A` termid) `A` I 100)

testlet = eval env0 termlet