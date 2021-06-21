namespace algebra

class SYM (r : Type*) :=
  (lit : bool -> r)
  (neg : r -> r)
  (and : r -> r -> r)
  (or  : r -> r -> r)

prefix `¬` := SYM.neg
infix `&&` := SYM.and
infix `||` := SYM.or

open SYM

instance R : SYM bool := {
  lit := id,
  neg := bnot,
  and := (&&),
  or  := (||),
}


instance : SYM string := 
let paren (s : string) := "(" ++ s ++ ")" in
{
  lit := λ x, match x with
              | tt := "tt"
              | ff := "ff"
              end,
  neg := λ x, "¬" ++ x,
  and := λ x y, paren (x ++ " && " ++ y),
  or  := λ x y, paren (x ++ " || " ++ y),
}

open SYM

inductive Expr
| Lit : bool -> Expr
| Neg : Expr -> Expr
| And : Expr -> Expr -> Expr
| Or  : Expr -> Expr -> Expr

open Expr
instance : SYM Expr := {
  lit := Lit,
  neg := Neg,
  and := And,
  or  := Or,
}

def ex0 : Expr := ¬ (lit ff)
def ex1 : Expr := lit tt && ¬ (lit ff)

def eval {r : Type} [i: SYM r] : Expr -> r
| (Lit e) := lit e
| (Neg e) := neg (eval e)
| (And e1 e2) := and (eval e1) (eval e2)
| (Or  e1 e2) := or (eval e1) (eval e2)

instance : has_repr Expr := ⟨eval⟩

#eval (eval ex0 : Expr)
#eval (eval ex0 : bool)
#eval (eval ex1 : string)

end algebra