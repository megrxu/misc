import tactic control.functor control.applicative
import init.meta.smt.rsimp

open expr rsimp

namespace meta_playground

meta def size : expr → nat
| (app f a) := size f + size a
| _ := 1

meta def choose (ccs : cc_state) (e : expr) : expr :=
  ccs.fold_eqc e e $ λ (best_so_far curr : expr),
    if size curr < size best_so_far then curr else best_so_far

meta def rsimp : tactic unit :=
  do ccs ← collect_implied_eqs,
    tactic.try $ tactic.simp_top_down $ λ t, do
      let root := ccs.root t,
      let t'   := choose ccs root,
      p        ←  ccs.eqv_proof t t',
      return (t', p)

constants(f : nat→nat→nat) (g : nat→nat) (p : nat → nat → Prop)

@[simp]
theorem fax : ∀ x, f x x = x := sorry

axioms (pax : ∀ x, p x x)

example(a b c : nat) (h1: a=g b) (h2: a=b) : p (f (g a) a) b:=
  sorry

end meta_playground