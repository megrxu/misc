namespace algebra

class SYM (r : Type*) :=
  (lit : bool -> r)
  (neg : r -> r)
  (and : r -> r -> r)
  (or  : r -> r -> r)


def R : SYM bool := {
  lit := id,
  neg := bnot,
  and := (&&),
  or  := (||),
}

open SYM

def S : SYM string := 
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

def ex0 {r : Type} (i : SYM r): r := SYM.neg (SYM.lit ff)

#eval (ex0 R)
#eval (ex0 S)

-- Rank N types
coinductive Sym : Type
| mk : (forall (r : Type), SYM r -> r) -> Sym

instance : SYM Sym := sorry

end algebra