import Control.Monad.State (State, get, put, runState)
import qualified Data.IntMap as M
import Data.List (nub)
import Distribution.PackageDescription.Configuration (freeVars)
import TypeInferChurchMonad (TVE (..), TVEM, TVarName, Term (..), Typ (..), VarName, termlet, newtv, occurs, tve0, tvext, tvsub, unifyM)
import Untyped (env0, ext, lkup)

data TypS = TypS [TVarName] Typ
  deriving (Show, Eq)

type TEnv = [(VarName, TypS)]

instantiate :: TypS -> TVEM Typ
instantiate (TypS tvs t) = do
  tve <- associate_with_freshvars tvs
  return $ tvsub tve t
  where
    associate_with_freshvars [] = return tve0
    associate_with_freshvars (tv : tvs) = do
      tve <- associate_with_freshvars tvs
      tvfresh <- newtv
      return $ tvext tve (tv, tvfresh)

example1 = instantiate (TypS [1] TInt)

generalize :: TVEM Typ -> TVEM TypS
generalize ta = do
  tve_before <- get
  t <- ta
  tve_after <- get
  let t' = tvsub tve_after t
  let tvdep = tvdependentset tve_before tve_after
  let fv = filter (not . tvdep) (nub (freevers t'))
  return (TypS fv t')

freevers :: Typ -> [TVarName]
freevers TInt = []
freevers (t1 :> t2) = freevers t1 ++ freevers t2
freevers (TV v) = [v]

tvdependentset :: TVE -> TVE -> (TVarName -> Bool)
tvdependentset tve_before tve_after =
  \tv -> any (\tvb -> occurs tv (TV tvb) tve_after) tvbs
  where
    tvbs = tvfree tve_before

tvfree :: TVE -> [TVarName]
tvfree (TVE c s) = filter (\v -> not (M.member v s)) [0 .. c -1]

teval' :: TEnv -> Term -> TVEM Typ
teval' env (V x) = instantiate (lkup env x)
teval' env (L x e) = do
  tv <- newtv
  te <- teval' (ext env (x, TypS [] tv)) e
  return (tv :> te)
teval' env (Let (x, e) eb) = do
  t <- generalize (teval' env e)
  teval' (ext env (x, t)) eb
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

teval :: TEnv -> Term -> TypS
teval tenv e =
  let (ts, _) = runState (generalize (teval' tenv e)) tve0 in ts

(vx, vy) = (V "x", V "y")

term1 = L "x" (IFZ vx (I 1) (vx :+ I 2))

test1 = teval env0 term1

testlet = teval env0 termlet

term2a = L "x" (L "y" (vx `A` vy))
test2a = teval env0 term2a