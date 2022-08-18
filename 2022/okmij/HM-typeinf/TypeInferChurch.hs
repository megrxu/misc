module TypeInferChurch where

import qualified Data.IntMap as M
import Untyped (env0, ext, lkup)

type TVarName = Int

data Typ = TInt | Typ :> Typ | TV TVarName
  deriving (Show, Eq)

infixr 9 :>

type VarName = String

data Term
  = V VarName
  | L VarName Term
  | A Term Term
  | I Int
  | Term :+ Term
  | Fix Term
  | IFZ Term Term Term
  deriving (Show, Eq)

type TEnv = [(VarName, Typ)]

data TVE = TVE Int (M.IntMap Typ)

newtv :: TVE -> (Typ, TVE)
newtv (TVE n s) = (TV n, TVE (succ n) s)

tve0 :: TVE
tve0 = TVE 0 M.empty

tvlkup :: TVE -> TVarName -> Maybe Typ
tvlkup (TVE _ s) v = M.lookup v s

tvext :: TVE -> (TVarName, Typ) -> TVE
tvext (TVE c s) (tv, t) = TVE c (M.insert tv t s)

tvsub :: TVE -> Typ -> Typ
tvsub tve (t1 :> t2) = tvsub tve t1 :> tvsub tve t2
tvsub tve (TV v) | Just t <- tvlkup tve v = tvsub tve t
tvsub tve t = t

unify :: Typ -> Typ -> TVE -> Either String TVE
unify t1 t2 tve = unify' (tvchase tve t1) (tvchase tve t2) tve

tvchase :: TVE -> Typ -> Typ
tvchase tve (TV v) | Just t <- tvlkup tve v = tvchase tve t
tvchase _ t = t

unify' :: Typ -> Typ -> TVE -> Either String TVE
unify' TInt TInt = Right
unify' (t1a :> t1r) (t2a :> t2r) = either Left (unify t1r t2r) . unify t1a t2a
unify' t1 (TV v2) = unifyv v2 t1
unify' (TV v1) t2 = unifyv v1 t2
unify' t1 t2 =
  const
    ( Left $
        unwords
          [ "constant mismatch:",
            show t1,
            "and",
            show t2
          ]
    )

unifyv :: TVarName -> Typ -> TVE -> Either String TVE
unifyv v1 (TV v2) tve =
  if v1 == v2
    then Right tve
    else Right (tvext tve (v1, TV v2))
unifyv v1 t2 tve =
  if occurs v1 t2 tve
    then
      Left $
        unwords
          [ "occurs check failed:",
            show (TV v1),
            "in",
            show (tvsub tve t2)
          ]
    else Right (tvext tve (v1, t2))

occurs :: TVarName -> Typ -> TVE -> Bool
occurs v TInt _ = False
occurs v (t1 :> t2) tve = occurs v t1 tve || occurs v t2 tve
occurs v (TV v2) tve =
  case tvlkup tve v2 of
    Nothing -> v == v2
    Just t -> occurs v t tve

teval' :: TEnv -> Term -> (TVE -> (Typ, TVE))
teval' env (V x) = \tve0 -> (lkup env x, tve0)
teval' env (L x e) = \tve0 ->
  let (tv, tve1) = newtv tve0
      (te, tve2) = teval' (ext env (x, tv)) e tve1
   in (tv :> te, tve2)
teval' env (A e1 e2) = \tve0 ->
  let (t1, tve1) = teval' env e1 tve0
      (t2, tve2) = teval' env e2 tve1
      (t1r, tve3) = newtv tve2
   in case unify t1 (t2 :> t1r) tve3 of
        Right tve -> (t1r, tve)
        Left err -> error err
teval' env (I n) = \tve0 -> (TInt, tve0)
teval' env (e1 :+ e2) = \tve0 ->
  let (t1, tve1) = teval' env e1 tve0
      (t2, tve2) = teval' env e2 tve1
   in case either Left (unify t2 TInt) . unify t1 TInt $ tve2 of
        Right tve -> (TInt, tve)
        Left err -> error $ "Tring to add non-integers: " ++ err
teval' env (IFZ e1 e2 e3) = \tve0 ->
  let (t1, tve1) = teval' env e1 tve0
      (t2, tve2) = teval' env e2 tve1
      (t3, tve3) = teval' env e3 tve2
   in case unify t1 TInt tve3 of
        Right tve -> case unify t2 t3 tve of
          Right tve -> (t2, tve)
          Left err ->
            error $
              unwords
                [ "Branches of IFZ have",
                  "different types.",
                  "Unification failed:",
                  err
                ]
        Left err -> error $ "Tring to compare a non-integer to 0: " ++ err
teval' env (Fix e) = \tve0 ->
  let (t1, tve1) = teval' env e tve0
      (ta, tve2) = newtv tve1
      (tb, tve3) = newtv tve2
   in case unify t1 ((ta :> tb) :> (ta :> tb)) tve3 of
        Right tve -> (ta :> tb, tve)
        Left err -> error ("Inapproproate type in Fix: " ++ err)

teval :: TEnv -> Term -> Typ
teval tenv e = let (t, tve) = teval' tenv e tve0 in tvsub tve t

(vx, vy) = (V "x", V "y")

term1 = L "x" (IFZ vx (I 1) (vx :+ I 2))

test11 = teval env0 term1

test12 = teval env0 (term1 `A` I 2)

term3 = L "x" (IFZ vx (I 1) vy)

test32 = teval env0 (term3 `A` I 0)

test33 = teval env0 (term3 `A` I 1)

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

testm1 = teval env0 (tmul1 `A` I (-1) `A` I (-1))

delta = L "y" (vy `A` vy)

-- termY = L "f" (delta `A` L "y" (L "x" (vf `A` (delta `A` vy) `A` vx)))

tmul =
  Fix
    ( L
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
    )

testm21 = teval env0 tmul

testm24 = teval env0 (tmul `A` I (-1) `A` I (-1))

term2a = L "x" (L "y" (vx `A` vy))

test2a = teval' env0 term2a

termid = L "x" vx

testid = teval env0 termid

term2id = L "f" (L "y" (I 2 :+ ((termid `A` V "f") `A` ((termid `A` vy) :+ I 1))))

test2id = teval env0 term2id

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

testlet = teval env0 termlet