namespace run

class Language (a h : Type) (r : Type* -> Type* -> Type*) :=
  (here   : r (a × h) a)
  (before : Π any : Type, r h a -> r (any × h) a)
  (lambda : Π b : Type, r (a × h) b -> r h (a -> b))
  (apply  : Π b : Type, r h (a -> b) -> (r h a -> r h b))
  (loop   : r h (a -> a) -> r h a)
  (int    : ℤ -> r h ℤ)
  (up     : r h ℤ -> r h ℤ)
  (down   : r h ℤ -> r h ℤ)
  (add    : r h ℤ -> r h ℤ -> r h ℤ)
  (mult   : r h ℤ -> r h ℤ -> r h ℤ)
  (gte    : r h ℤ -> r h ℤ -> r h bool)
  (ifte   : r h bool -> r h a -> r h a -> r h a)
  (neg    : r h bool -> r h bool)
  (or     : r h bool -> r h bool -> r h bool)
  (and    : r h bool -> r h bool -> r h bool)
  (bool   : bool -> r h bool)

structure L (h a : Type) := {
  run : h -> a
}

def arity0 {h a : Type} (x : a) : L h a := {
  L . run := λ _, x
}
def arity1 {h a b: Type} (op : a -> b) (e: L h a) : L h b := {
  L . run := λ x, op (e.run x)
}
def arity2 {h a b c: Type} (op : a -> b -> c) (e1: L h a) (e2: L h b) : L h c := {
  L . run := λ x, op (e1.run x) (e2.run x)
}

def Lmk (h a : Type) : Type := L h a

def apply {a b : Type} (f : a -> b) (arg : a) : b := f arg

instance (a h : Type) : Language a h Lmk := {
  here   := { L . run := prod.fst},
  before := λ any x, { L . run := x.run ∘ prod.snd},
  lambda := λ b e, {L . run := λ x y, e.run (y, x)},
  apply  := λ b, arity2 apply,

  loop   := λ x, { L . run := sorry },

  int    := arity0,
  add    := arity2 (+),
  mult   := arity2 (*),
  gte    := arity2 ((λ x y, to_bool (x ≥ y)) : int -> int -> bool),
  up     := arity1 (λ x, x + 1),
  down   := arity1 (λ x, x - 1),

  ifte   := λ e1 e2 e3,
            { L . run := λ x, if e1.run x
                              then e2.run x
                              else e3.run x },
  neg    := arity1 bnot,
  or     := arity2 bor,
  and    := arity2 band,
  bool   := arity0,
}

namespace Language

def term (a : Type) := forall r h, Language a h r → r h a

end Language

end run