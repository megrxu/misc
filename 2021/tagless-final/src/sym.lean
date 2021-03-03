namespace algebra

class SYM (r : Type) :=
  (lit : bool -> r)
  (neg : r -> r)
  (and : r -> r -> r)
  (or  : r -> r -> r)


instance SYM_bool : SYM bool := {
  lit := id,
  neg := bnot,
  and := (&&),
  or  := (||),
}

inductive R : Type | R : bool -> R
def deR (x : R) : bool := match x with
  | R.R b := b
  end
instance : has_repr R := ⟨λ s, repr (deR s)⟩

def wrapped₂ {a b : Type} (f : a -> b) (g : b -> a)
      (app₂ : a -> a -> a) : (b -> b -> b) := λ x y, f (app₂ (g x) (g y))

instance : SYM R := {
  lit := R.R,
  neg := λ x, R.R (bnot (deR x)),
  and := wrapped₂ R.R deR (&&),
  or  := wrapped₂ R.R deR (||),
}

inductive S : Type | S : string -> S
def deS (x : S) : string := match x with
  | S.S s := s
  end
instance : has_repr S := ⟨λ s, deS s⟩

instance : SYM S := {
  lit := λ x, match x with 
              | tt := S.S "tt"
              | ff := S.S "ff"
              end,
  neg := λ x, S.S (string.append "~" (deS x)),
  and := λ x y, S.S (string.append (deS x) (string.append "&&" (deS y))),
  or  := λ x y, S.S (string.append (deS x) (string.append "||" (deS y))),
}
def paren (x : S) : S := S.S (string.append "(" (string.append (deS x) ")" ))

def ex0 (r : Type) [SYM r]: r := SYM.neg (SYM.lit ff)
#eval ex0 R
#eval ex0 S

-- Rank N types not implemented
-- def Sym {r : Type} : Type
-- | Sym [SYM r] : r -> Sym := 
-- def deSym {r : Type} (x : Sym) : r := match x with
--   | Sym.Sym s := s
--   end
-- instance (r : Type) [has_repr r] : has_repr Sym := ⟨λ s, repr (deSym s)⟩

-- instance (r : Type) [SYM r] : SYM (Sym r)  := {
--   lit := λ x, Sym.Sym (SYM.lit x),
--   neg := λ x, Sym.Sym (SYM.neg (deSym x)),
--   and := λ x y, Sym.Sym (SYM.and (deSym x) (deSym y)),
--   or  := λ x y, Sym.Sym (SYM.or  (deSym x) (deSym y)),
-- }

end algebra