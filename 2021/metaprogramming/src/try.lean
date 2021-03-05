import tactic control.functor control.applicative

open tactic

meta def find_absurd_proof (e : expr) (ctx : list expr) : tactic unit := 
do prf <- (ctx.mfirst (λ h, to_expr ``(%%e %%h))),
   exact prf

meta def tactic.interactive.contr : tactic unit :=
do exfalso,
   ctx ← local_context,
   ctx.mfirst (λ e, find_absurd_proof e ctx)

example (P Q R : Prop) (hp : P) (hq : Q) (hr : ¬ R) (hnq : ¬ Q) : false :=
by contr


example (P Q R : Prop) (hnq : ¬ Q) (hp : P) (hq : Q) (hr : ¬ R) : 0 = 1 :=
by contr


example (P Q R : Prop) (hp : P) (hq : Q) (hr : ¬ R) (hnq : Q → false) : false :=
by contr



/-!

## Exercise 2

Write a tactic that proves a given `nat`-valued declaration is nonnegative.
The tactic should take the name of a declaration whose return type is `ℕ`
(presumably with some arguments), e.g. `nat.add : ℕ → ℕ → ℕ`
or `list.length : Π α : Type, list α → ℕ`.
It should add a new declaration to the environment which proves all applications
of this function are nonnegative,
e.g. `nat.add_nonneg : ∀ m n : ℕ, 0 ≤ nat.add m n`.

Bonus: create reasonable names for these declarations, and/or take an optional argument
for the new name.

This tactic is not useful by itself, but it's a good way to practice
querying and modifying an environment and working under binders.
It is not a tactic to be used during a proof, but rather as a command.


Hints:
* For looking at declarations in the environment, you will need the `declaration` type,
  as well as the tactics `get_decl` and `add_decl`.
* You will have to manipulate an expression under binders.
  The tactics `mk_local_pis` and `pis`, or their lambda equivalents, will be helpful here.
* `mk_mapp` is a variant of `mk_app` that lets you provide implicit arguments.
-/


meta def add_nonneg_proof (n : name) : tactic unit :=

   -- first we find the declaration named `n` in the environment.
do d ← get_decl n,

   -- the type of d is `Π x y z ..., body`,
   -- where body contains a bunch of free variables.
   -- we instantiate the binders to get a body we can manipulate.
   (args, body) ← mk_local_pis d.type,

   -- args is a list of expressions, but we want a list of `option expr`s to give to `mk_mapp`.
   let args_with_some := args.map some,

   -- this line applies the expression named `n` to the variables we've created.
   -- d_body is the natural number that we want to prove is nonnegative.
   d_body ← mk_mapp n args_with_some,

   -- so we prove that `d_body` is nonnegative by applying `nat.zero_le`.
   nonneg_prf_body ← mk_app `nat.zero_le [d_body],

   -- now we abstract away the local constants we created.
   nonneg_prf ← lambdas args nonneg_prf_body,

   -- we create a name for our new proof.
   -- if `n` is `nat.add, we will call our new proof `nat.add.nonneg
   let new_decl_name := n.append `nonneg,

   -- we get the type of the proof we've constructed,
   decl_tp ← infer_type nonneg_prf,

   -- make a term of type `declaration`,
   let new_decl := mk_theorem new_decl_name d.univ_params decl_tp nonneg_prf,

   -- and add that declaration to the environment.
   add_decl new_decl




run_cmd add_nonneg_proof `nat.add
run_cmd add_nonneg_proof `list.length

#check nat.add.nonneg
#check list.length.nonneg


/-!

## Exercise 3 (challenge!)

The mathlib tactic `cancel_denoms` is intended to get rid of division by numerals
in expressions where this makes sense. For example,

-/

example (q : ℚ) (h : q / 3 > 0) : q > 0 :=
begin
  cancel_denoms at h, exact h
end

/-!

But it is not complete. In particular, it doesn't like nested division
or other operators in denominators. These all fail:

-/

example (q : ℚ) (h : q / (3 / 4) > 0) : false :=
begin
  -- cancel_denoms at h,
  admit
end

example (p q : ℚ) (h : q / 2 / 3 < q) : false :=
begin
  -- cancel_denoms at h,
  admit
end

example (p q : ℚ) (h : q / 2 < 3 / (4*q)) : false :=
begin
  -- cancel_denoms at h,
  admit
end

-- this one succeeds but doesn't do what it should
example (p q : ℚ) (h : q / (2*3) < q) : false :=
begin
  -- cancel_denoms at h,
  admit
end

/-!

Look at the code in `src/tactic/cancel_denoms.lean` and try to fix it.
See if you can solve any or all of these failing test cases.

If you succeed, a pull request to mathlib is strongly encouraged!

-/
