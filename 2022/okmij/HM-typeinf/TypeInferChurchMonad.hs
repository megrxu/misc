{-# LANGUAGE TupleSections #-}

module TypeInferChurchMonad where

import Control.Monad.State (State, fail, get, put, runState)
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
  | Let (VarName, Term) Term
  deriving (Show, Eq)

type TEnv = [(VarName, TVEM Typ)]

data TVE = TVE Int (M.IntMap Typ)

type TVEM = State TVE

newtv :: TVEM Typ
newtv = do
  TVE n s <- get
  put (TVE (succ n) s)
  return (TV n)

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

unifyM :: Typ -> Typ -> (String -> String) -> TVEM ()
unifyM t1 t2 errf = do
  tve <- get
  case unify t1 t2 tve of
    Right tve -> put tve
    Left err -> error (errf err)

teval' :: TEnv -> Term -> TVEM Typ
teval' env (V x) = lkup env x
teval' env (L x e) = do
  tv <- newtv
  te <- teval' (ext env (x, return tv)) e
  return $ tv :> te
teval' env (A e1 e2) = do
  t1 <- teval' env e1
  t2 <- teval' env e2
  t1r <- newtv
  unifyM t1 (t2 :> t1r) id
  return t1r
teval' env (I n) = return TInt
teval' env (e1 :+ e2) = do
  t1 <- teval' env e1
  t2 <- teval' env e2
  unifyM t1 TInt ("Trying to add non-integers: " ++)
  unifyM t2 TInt ("Trying to add non-integers: " ++)
  return TInt
teval' env (IFZ e1 e2 e3) = do
  t1 <- teval' env e1
  t2 <- teval' env e2
  t3 <- teval' env e3
  unifyM t1 TInt ("Tring to compare a non-integer to 0: " ++)
  unifyM t2 t3 ("Branches of IFZ have different types. Unification failed: " ++)
  return t2
teval' env (Fix e) = do
  t1 <- teval' env e
  ta <- newtv
  tb <- newtv
  unifyM t1 ((ta :> tb) :> (ta :> tb)) ("Inapproproate type in Fix: " ++)
  return (ta :> tb)
teval' env (Let (x, e) eb) = do
  _ <- teval' env e
  teval' (ext env (x, teval' env e)) eb

teval :: TEnv -> Term -> Typ
teval tenv e = let (t, tve) = runState (teval' tenv e) tve0 in tvsub tve t

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

termY = L "f" (delta `A` L "y" (L "x" (V "f" `A` (delta `A` vy) `A` vx)))

tmulf =
  L
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

tmul = Fix tmulf

testm20 = teval env0 tmulf

testm21 = teval env0 tmul

testm24 = teval env0 (tmul `A` I (-1) `A` I (-1))

term2a = L "x" (L "y" (vx `A` vy))

test2a = teval' env0 term2a

termid = L "x" vx

testid = teval env0 termid

term2id =
  Let
    ("termid", L "x" vx)
    ( L
        "f"
        ( L
            "y"
            ( I 2
                :+ ( (V "termid" `A` V "f")
                       `A` ((V "termid" `A` vy) :+ I 1)
                   )
            )
        )
    )

term2id' =
  L
    "f"
    ( L
        "y"
        ( I 2
            :+ ( (L "x" vx `A` V "f")
                   `A` ((L "x" vx `A` vy) :+ I 1)
               )
        )
    )

test2id = teval env0 term2id

testl1 = teval env0 $ Let ("x", vx) vx

testl2 = teval env0 $ Let ("x", vy) (I 1)

testl3 = teval env0 $ Let ("x", I 1) (vx :+ vx)

termlet =
  Let
    ("c2", L "f" (L "x" (V "f" `A` (V "f" `A` vx))))
    ( Let
        ("inc", L "x" (vx :+ I 1))
        ( Let
            ( "compose",
              L
                "f"
                ( L
                    "g"
                    ( L
                        "x"
                        (V "f" `A` (V "g" `A` vx))
                    )
                )
            )
            (V "c2")
            `A` (V "compose" `A` V "inc" `A` V "inc")
            `A` I 10
            :+ ((V "c2" `A` (V "compose" `A` V "inc") `A` termid) `A` I 100)
        )
    )

testlet = teval env0 termlet
