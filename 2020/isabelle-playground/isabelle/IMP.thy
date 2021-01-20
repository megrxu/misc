theory IMP
  imports Main

begin

subsection \<open>Arithmetic Expressions\<close>

type_synonym vname = string
datatype aexp = N int | V vname | Plus aexp aexp

type_synonym val = int
type_synonym state = "vname \<Rightarrow> val"

fun aval :: "aexp \<Rightarrow> state \<Rightarrow> val" where
  "aval (N n) s = n" |
  "aval (V x) s = s x" |
  "aval (Plus a1 a2) s = aval a1 s + aval a2 s"

(* assign values to vars *)
value "aval (Plus (N 3) (V ''y'')) (((\<lambda>x.1) (''x'' := 7)) (''y'' := 9))"

(* constant folding *)
fun asimp_const :: "aexp \<Rightarrow> aexp" where
  "asimp_const (N n) = N n" |
  "asimp_const (V x) = V x" |
  "asimp_const (Plus a1 a2) = (case (asimp_const a1, asimp_const a2) of 
                                (N n1, N n2) \<Rightarrow> N (n1 + n2) | 
                                (b1, b2) \<Rightarrow> Plus b1 b2)"

(* split *)
lemma aval_eq :"aval (asimp_const a) s = aval a s"
  by (induction a) (auto split: aexp.split)

fun plus :: "aexp \<Rightarrow> aexp \<Rightarrow> aexp" where
  "plus (N i1) (N i2) = N (i1 + i2)" |
  "plus (N i) a = (if i = 0 then a else Plus (N i) a)" | 
  "plus a (N i) = (if i = 0 then a else Plus a (N i))" |
  "plus a1 a2 = Plus a1 a2"

lemma aval_plus : "aval (plus a1 a2) s = aval a1 s + aval a2 s"
  by (induction a1 a2 rule: plus.induct) auto

fun asimp :: "aexp \<Rightarrow> aexp" where
  "asimp (N n) = N n" |
  "asimp (V x) = V x" |
  "asimp (Plus a1 a2) = plus (asimp a1) (asimp a2)"

lemma aval_asimp: "aval (asimp t) s = aval t s"
  by (induction t) (auto simp add: aval_plus)

(* 3.1 *)
fun optimal :: "aexp \<Rightarrow> bool" where
  "optimal (N x) = True" | 
  "optimal (V y) = True" |
  "optimal (Plus (N i) (N j)) = False" |
  "optimal (Plus a b) = conj (optimal a) (optimal b)"

theorem  opti [simp]: "optimal (asimp_const a)"
  by (induction a) (auto split: aexp.split)

(* 3.2 *)
fun sumN :: "aexp \<Rightarrow> int" where
  "sumN (N n) = n" |
  "sumN (V x) = 0" |
  "sumN (Plus a b) = sumN a + sumN b"

fun zeroN :: "aexp \<Rightarrow> aexp" where
  "zeroN (N n) = N 0" |
  "zeroN (V x) = V x" |
  "zeroN (Plus a b) = plus (asimp_const (zeroN a)) (asimp_const (zeroN b))"

value "full_asimp (Plus (N 1) (Plus (V x ) (Plus (N 2) (V x))))"

definition sepN :: "aexp \<Rightarrow> aexp" where
  "sepN m = plus (zeroN m) (N (sumN m))"

lemma aval_sepN: "aval (sepN t) s = aval t s"
  by (induction t) (auto simp add: sepN_def aval_eq aval_plus)

fun full_simp :: "aexp \<Rightarrow> aexp" where
  "full_simp m = asimp (sepN m)"

value "full_simp (Plus (N 1) (Plus (V x ) (N 2)))"

theorem aval_full_simp: "aval (full_simp t) s = aval t s"
  by (induction t) (auto simp add: aval_asimp aval_plus aval_sepN)

(* 3.3 *)
fun subst :: "vname \<Rightarrow> aexp \<Rightarrow> aexp \<Rightarrow> aexp" where
  "subst x a (V ve) = (if ve = x then a else (V ve))" |
  "subst x a (Plus e1 e2) = Plus (subst x a e1) (subst x a e2)" |
  "subst x a e = e"

value "subst ''x'' (N 3) (Plus (V ''x'') (V ''y''))"

lemma subst_lemma: "aval (subst x a e) s = aval e (s(x := aval a s))"
  by (induction e) (auto)

lemma "aval a1 s = aval a2 s \<Longrightarrow> aval (subst x a1 e) s = aval (subst x a2 e) s"
  by (auto simp add: subst_lemma)

(* 3.6 *)
datatype lexp = Nl int | Vl vname | Plusl lexp lexp | LET vname lexp lexp

fun lval :: "lexp \<Rightarrow> state \<Rightarrow> int" where
  "lval (Nl n) s = n" |
  "lval (Vl x) s = s x" |
  "lval (Plusl a b) s = lval a s + lval b s" |
  "lval (LET x e1 e2) s = (lval e2 (s(x := lval e1 s)))"

value "lval (LET ''x'' (Plusl (Vl ''x'') (Vl ''x'')) (Plusl (Vl ''x'') (Nl 2))) (\<lambda>x.2)"

fun inline :: "lexp \<Rightarrow> aexp" where
  "inline (Nl n) = N n" |
  "inline (Vl v) = V v" |
  "inline (Plusl a b) = Plus (inline a) (inline b)" |
  "inline (LET x e1 e2) = subst x (inline e1) (inline e2)"

lemma eval_inline: "lval le = aval (inline le)"
  by (induction le rule: inline.induct) (auto simp add: subst_lemma)

(* 3.5 *)
(*
datatype aexp2 = N int | V vname | Plus aexp2 aexp2 | Vpp vname | Div aexp2 aexp2

fun aval2 :: "aexp2 \<Rightarrow> state \<Rightarrow> val \<times> state" where
  "aval2 (N n) s = (n, s)" |
  "aval2 (V x) s = (s x, s)" |
  "aval2 (Plus a b) s = (fst (aval2 a s) + fst(aval2 b (snd (aval2 a s))), snd(aval2 b (snd (aval2 a s))))" |
  "aval2 (Div a b) s = ((fst (aval2 a s)) div (fst(aval2 b (snd (aval2 a s)))), snd(aval2 b (snd (aval2 a s))))" |
  "aval2 (Vpp x) s = (s x, \<lambda>x. s x + 1)"

value "aval2 (Plus (Vpp ''x'') (Vpp ''x'' )) (\<lambda>x.1)"
value "snd (aval2 (Plus (Vpp ''x'') (Vpp ''x'' )) (\<lambda>x.1)) ''x''"
value "aval2 (Div (N 4) (N 2)) (\<lambda>x.1)"
value "aval2 (Plus (Div (N 4) (Vpp x)) (V x)) (\<lambda>x.1)"
*)

subsection \<open>Boolean Expressions\<close>

datatype bexp = Bc bool | Not bexp | And bexp bexp | Less aexp aexp

fun bval :: "bexp \<Rightarrow> state \<Rightarrow> bool" where
  "bval (Bc v) s = v" |
  "bval (Not b) s = (\<not> bval b s)" |
  "bval (And b1 b2) s = (bval b1 s \<and> bval b2 s)" |
  "bval (Less a1 a2) s = (aval a1 s < aval a2 s)"

fun not :: "bexp \<Rightarrow> bexp" where
  "not (Bc True) = Bc False" |
  "not (Bc False) = Bc True" |
  "not b = Not b"

fun "and" :: "bexp \<Rightarrow> bexp \<Rightarrow> bexp" where
"and (Bc True) b = b" |
"and b (Bc True) = b" |
"and (Bc False) b = Bc False" |
"and b (Bc False) = Bc False" |
"and b\<^sub>1 b\<^sub>2 = And b\<^sub>1 b\<^sub>2"

fun less :: "aexp \<Rightarrow> aexp \<Rightarrow> bexp" where
"less (N n\<^sub>1) (N n\<^sub>2) = Bc(n\<^sub>1 < n\<^sub>2)" |
"less a\<^sub>1 a\<^sub>2 = Less a\<^sub>1 a\<^sub>2"

fun bsimp :: "bexp \<Rightarrow> bexp" where
"bsimp (Bc v ) = Bc v" |
"bsimp (Not b) = not (bsimp b)" |
"bsimp (And b1 b2 ) = and (bsimp b1 ) (bsimp b2 )" |
"bsimp (Less a1 a2) = less (asimp a1) (asimp a2)"

(* 3.7 *)

fun Eq :: "aexp \<Rightarrow> aexp \<Rightarrow> bexp" where
  "Eq a1 a2 = And (Not (Less a1 a2)) (Not (Less a2 a1))"

fun Le :: "aexp \<Rightarrow> aexp \<Rightarrow> bexp" where
  "Le a1 a2 = Not (Less a2 a1)"

lemma "bval (Eq a1 a2 ) s = (aval a1 s = aval a2 s)"
  by auto

lemma "bval (Le a1 a2 ) s = (aval a1 s \<le> aval a2 s)"
  by auto

(* 3.8 *)

datatype ifexp = Bc2 bool | If ifexp ifexp ifexp | Less2 aexp aexp

fun ifval :: "ifexp \<Rightarrow> state \<Rightarrow> bool" where
  "ifval (Bc2 v) s = v" |
  "ifval (If a b c) s = (if (ifval a s) then (ifval b s) else (ifval c s))" |
  "ifval (Less2 a1 a2) s = ((aval a1 s) < (aval a2 s))"

fun if2bexp :: "ifexp \<Rightarrow> bexp" where
  "if2bexp (Bc2 v) = Bc v" |
  "if2bexp (Less2 a1 a2) = Less a1 a2" |
  "if2bexp (If a b c) = Not ((And (Not (And (if2bexp a) (if2bexp b))) (Not (And (Not (if2bexp a)) (if2bexp c)))))"

fun b2ifexp :: "bexp \<Rightarrow> ifexp" where
  "b2ifexp (Bc v) = Bc2 v" |
  "b2ifexp (Less a1 a2) = Less2 a1 a2" |
  "b2ifexp (And b1 b2) = If (b2ifexp b1) (b2ifexp b2) (Bc2 False)" |
  "b2ifexp (Not b) = If (b2ifexp b) (Bc2 False) (Bc2 True)"

lemma bval_b2if: "bval b s = ifval (b2ifexp b) s"
  by (induction b) (auto)

lemma bval_if2b: "ifval i s = bval (if2bexp i) s"
  by (induction i) (auto)

(* 3.9 *)

datatype pbexp = VAR vname | NOT pbexp | AND pbexp pbexp | OR pbexp pbexp

fun pbval :: "pbexp \<Rightarrow> (vname \<Rightarrow> bool) \<Rightarrow> bool" where
  "pbval (VAR x) s = s x" |
  "pbval (NOT b) s = (\<not> pbval b s)" |
  "pbval (AND b1 b2) s = (pbval b1 s \<and> pbval b2 s)" |
  "pbval (OR b1 b2) s = (pbval b1 s \<or> pbval b2 s)"

fun is_nnf :: "pbexp \<Rightarrow> bool" where
  "is_nnf (VAR v) = True" |
  "is_nnf (NOT (VAR v)) = True" |
  "is_nnf (AND p1 p2) = (is_nnf p1 \<and> is_nnf p2)" |
  "is_nnf (OR p1 p2) = (is_nnf p1 \<and> is_nnf p2)" |
  "is_nnf p = False"

fun nnf :: "pbexp \<Rightarrow> pbexp" where
  "nnf (NOT (AND p1 p2)) = OR (nnf (NOT p1)) (nnf (NOT p2))" |
  "nnf (NOT (OR p1 p2)) = AND (nnf (NOT p1)) (nnf (NOT p2))" |
  "nnf (AND p1 p2) = AND (nnf p1) (nnf p2)" |
  "nnf (OR p1 p2) = OR (nnf p1) (nnf p2)" |
  "nnf (NOT (NOT p)) = nnf p" |
  "nnf p = p"

lemma pval_nnf: "(pbval (nnf b) s = pbval b s)"
  by (induction b rule: nnf.induct) auto

lemma "(is_nnf (nnf b))"
  by (induction b rule: nnf.induct) auto

fun no_or :: "pbexp \<Rightarrow> bool" where
  "no_or (OR p1 p2) = False" |
  "no_or (AND p1 p2) = (no_or p1 \<and> no_or p2)" |
  "no_or (NOT p) = no_or p" |
  "no_or (VAR v) = True"

lemma [simp]: "is_nnf (NOT x2) \<Longrightarrow> no_or x2"
  by (induction rule:is_nnf.induct) auto

fun nnf_is_dnf :: "pbexp \<Rightarrow> bool" where
  "nnf_is_dnf (AND p1 p2) = (no_or p1 \<and> no_or p2)" |
  "nnf_is_dnf (OR p1 p2) = (nnf_is_dnf p1 \<and> nnf_is_dnf p2)" |
  "nnf_is_dnf (NOT p) = nnf_is_dnf p" |
  "nnf_is_dnf (VAR v) = True"

fun is_dnf :: "pbexp \<Rightarrow> bool" where 
  "is_dnf p = (is_nnf p \<and> nnf_is_dnf p)"

fun dist_and_over_or :: "pbexp \<Rightarrow> pbexp" where
  "dist_and_over_or (AND p1 (OR p2 p3)) = OR (dist_and_over_or (AND p1 p2)) (dist_and_over_or (AND p1 p3))" |
  "dist_and_over_or (AND (OR p1 p2) p3) = OR (dist_and_over_or (AND p1 p3)) (dist_and_over_or (AND p2 p3))" |
  "dist_and_over_or p = p"

lemma [simp]:
  "is_nnf x \<Longrightarrow> is_nnf (dist_and_over_or x)"
  by (induction "x" rule:dist_and_over_or.induct) auto

lemma [simp]:
  "nnf_is_dnf x \<Longrightarrow> nnf_is_dnf (dist_and_over_or x)"
  by (induction "x" rule:dist_and_over_or.induct) auto

lemma pbval_dist_and_over_or:
  "pbval (dist_and_over_or b) s = pbval b s"
  by (induction rule:dist_and_over_or.induct) auto

fun dnf_of_nnf :: "pbexp \<Rightarrow> pbexp" where
  "dnf_of_nnf (VAR v) = VAR v" |
  "dnf_of_nnf (NOT p) = NOT p" |
  "dnf_of_nnf (OR p1 p2) = (OR (dnf_of_nnf p1) (dnf_of_nnf p2))" |
  "dnf_of_nnf (AND p1 p2) = (case (dnf_of_nnf p1, dnf_of_nnf p2) of 
                                    (OR p11 p12, OR p21 p22) \<Rightarrow> OR (OR (dist_and_over_or (AND p11 p21)) (dist_and_over_or (AND p11 p22)))
                                                                   (OR (dist_and_over_or (AND p12 p21)) (dist_and_over_or (AND p12 p22)))
                                  | (OR p11 p12, p2') \<Rightarrow> OR (dist_and_over_or (AND p11 p2')) (dist_and_over_or (AND p12 p2'))
                                  | (p1', OR p21 p22) \<Rightarrow> OR (dist_and_over_or (AND p1' p21)) (dist_and_over_or (AND p1' p22)) 
                                  | (p1', p2') \<Rightarrow> AND p1' p2')"

lemma "(pbval (dnf_of_nnf b) s = pbval b s)"
  by (induction rule:dnf_of_nnf.induct) (auto split:pbexp.split simp add: pbval_dist_and_over_or)

value "dnf_of_nnf ( AND (VAR s\<^sub>1) (AND (OR (VAR s\<^sub>1) (VAR s\<^sub>1)) (VAR s\<^sub>1)))"

lemma aux0:
  assumes "nnf_is_dnf y" and "is_nnf y"
  assumes "nnf_is_dnf x" and "is_nnf x"
  shows "nnf_is_dnf (dist_and_over_or (AND x y))"
  using assms
  by (induction x rule:nnf_is_dnf.induct)(induction y rule:nnf_is_dnf.induct, auto)+

lemma aux1:
  "is_dnf (dnf_of_nnf p1) \<Longrightarrow> is_dnf (dnf_of_nnf p2) \<Longrightarrow> is_dnf (dnf_of_nnf (AND p1 p2))"
  by (auto split:pbexp.split simp add: aux0)

lemma "(is_nnf b \<Longrightarrow> is_dnf (dnf_of_nnf b))"
proof(induction "b" rule: is_nnf.induct)
  case (3 p1 p2)
  then show ?case 
    by (intro aux1) auto
qed auto

(* my solution *)

fun nnf_is_dnf' :: "pbexp \<Rightarrow> bool" where
  "nnf_is_dnf' (OR a b) = (nnf_is_dnf' a \<and> nnf_is_dnf' b)" |
  "nnf_is_dnf' (AND a b) = (no_or a \<and> no_or b)" |
  "nnf_is_dnf' (NOT a) = (no_or a)" |
  "nnf_is_dnf' a = (no_or a)"

definition is_dnf' :: "pbexp \<Rightarrow> bool" where 
  "is_dnf' p = (is_nnf p \<and> nnf_is_dnf' p)"

fun mk_dnf_conj :: "pbexp \<Rightarrow> pbexp \<Rightarrow> pbexp" where
"mk_dnf_conj e (OR y\<^sub>1 y\<^sub>2) = OR (mk_dnf_conj e y\<^sub>1) (mk_dnf_conj e y\<^sub>2)" |
"mk_dnf_conj (OR x\<^sub>1 x\<^sub>2) e = OR (mk_dnf_conj x\<^sub>1 e) (mk_dnf_conj x\<^sub>2 e)" |
"mk_dnf_conj x y = AND x y"

fun dnf_of_nnf' :: "pbexp \<Rightarrow> pbexp" where
"dnf_of_nnf' (VAR v)   = VAR v"                                     |
"dnf_of_nnf' (NOT e)   = NOT (dnf_of_nnf' e)"                        |
"dnf_of_nnf' (AND x y) = mk_dnf_conj (dnf_of_nnf' x) (dnf_of_nnf' y)" |
"dnf_of_nnf' (OR x y)  = OR (dnf_of_nnf' x) (dnf_of_nnf' y)"

(* prove that dnf_of_nnf' works *)
lemma nnf_after_mk: "is_nnf (AND a b) \<Longrightarrow> is_nnf (mk_dnf_conj a b)"
  by (induction a b rule: mk_dnf_conj.induct) auto
lemma nnf_not : "is_nnf (NOT b) \<Longrightarrow> is_nnf (NOT (dnf_of_nnf' b))"
  by (induction b) auto
lemma nnf_not_not: "is_nnf (NOT b) \<Longrightarrow> nnf_is_dnf' (dnf_of_nnf' b)"
  by (induction b) auto
lemma nnf_after_dnf: "is_nnf b \<Longrightarrow> is_nnf (dnf_of_nnf' b)"
  by (induction b) (auto simp add: nnf_after_mk nnf_not)
lemma no_or_not: "is_nnf (NOT b) \<Longrightarrow> no_or b"
  by induction auto
lemma no_or_not_dnf: "is_nnf (NOT b) \<Longrightarrow> no_or (dnf_of_nnf' b)"
  by (induction b) auto
lemma dnf_after_mk: "(is_dnf' a \<and> is_dnf' b) \<Longrightarrow> is_dnf' (mk_dnf_conj a b)"
  by (induction a b rule: mk_dnf_conj.induct) (auto simp add: is_dnf'_def no_or_not)
lemma no_or_dnf: "no_or a \<Longrightarrow> nnf_is_dnf' a"
  by induction auto
lemma maintain_nnf_is_dnf': "nnf_is_dnf' a \<Longrightarrow> nnf_is_dnf' b \<Longrightarrow> nnf_is_dnf' (mk_dnf_conj a b)"
  by (induction a b rule: mk_dnf_conj.induct) auto
lemma maintain_is_nnf: "is_nnf a \<Longrightarrow> nnf_is_dnf' (dnf_of_nnf' a)"
  by (induction a) (auto simp add: nnf_not no_or_not maintain_nnf_is_dnf')
lemma aux3: "is_nnf b1 \<Longrightarrow> is_nnf b2 \<Longrightarrow> nnf_is_dnf' (mk_dnf_conj (dnf_of_nnf' b1) (dnf_of_nnf' b2))"
  by (induction b1 b2 rule: mk_dnf_conj.induct ) (auto simp add: maintain_nnf_is_dnf' maintain_is_nnf no_or_not_dnf)

theorem "(is_nnf b \<Longrightarrow> is_dnf' (dnf_of_nnf' b))"
  by (induction b)  (auto simp add: is_dnf'_def  nnf_not nnf_after_mk nnf_not_not no_or_not_dnf aux3)

(* prove that dnf_of_nnf' has same value *)
lemma pbval_conj' : "pbval (mk_dnf_conj a b) s = (pbval a s \<and> pbval b s)"
  by (induction b rule: mk_dnf_conj.induct) auto
lemma "(pbval (dnf_of_nnf' b) s = pbval b s)"
  by (induction b rule: dnf_of_nnf'.induct) (auto simp add: pbval_conj')

section "Stack Machine and Compilation"

datatype instr = LOADI val | LOAD vname | ADD

type_synonym stack = "val list"

fun exec1 :: "instr \<Rightarrow> state \<Rightarrow> stack \<Rightarrow> stack" where
  "exec1 (LOADI n) _ stk  =  n # stk" |
  "exec1 (LOAD x) s stk  =  s(x) # stk" |
  "exec1 ADD _ (j # i # stk)  =  (i + j) # stk"

fun exec :: "instr list \<Rightarrow> state \<Rightarrow> stack \<Rightarrow> stack" where
  "exec [] _ stk = stk" |
  "exec (i#is) s stk = exec is s (exec1 i s stk)"

fun comp :: "aexp \<Rightarrow> instr list" where
  "comp (N n) = [LOADI n]" |
  "comp (V x) = [LOAD x]" |
  "comp (Plus e1 e2) = comp e1 @ comp e2 @ [ADD]"

lemma exec_append: "exec (is1 @ is2) s stk = exec is2 s (exec is1 s stk)"
  by (induction is1 arbitrary: stk) auto

(* compile an expression, and then execute it on a state, the result is to push the value of the expression to the stack*)
lemma "exec (comp a) s stk = aval a s # stk"
  by (induction a arbitrary: stk) (auto simp add: exec_append)

(* 3.10 *)

fun exec1' :: "instr \<Rightarrow> state \<Rightarrow> stack \<Rightarrow> stack option" where
  "exec1' (LOADI n) _ stk  =  Some (n # stk)" |
  "exec1' (LOAD x) s stk  =  Some (s(x) # stk)" |
  "exec1' ADD v (j # i # stk)  =  Some ((i + j) # stk)" |
  "exec1' ADD _ _ = None"

fun exec' :: "instr list \<Rightarrow> state \<Rightarrow> stack \<Rightarrow> stack option" where
  "exec' [] _ stk = Some (stk)" |
  "exec' (i#is) s stk = (case (exec1' i s stk) of
                (Some stk') \<Rightarrow> (exec' is s stk') |
                None \<Rightarrow> None)"

fun comp' :: "aexp \<Rightarrow> instr list" where
  "comp' (N n) = [LOADI n]" |
  "comp' (V x) = [LOAD x]" |
  "comp' (Plus e1 e2) = comp' e1 @ comp' e2 @ [ADD]"

lemma exec'_append: "exec' (is1 @ is2) s stk = (case (exec' is1 s stk) of 
                                                Some (stk') \<Rightarrow> exec' is2 s stk'
                                              | None \<Rightarrow> None)"
  by (induction is1 arbitrary: stk) (auto split: option.split)

lemma "exec' (comp a) s stk = Some (aval a s # stk)"
  by (induction a arbitrary: stk) (auto simp add: exec'_append)

(* 3.11 *)

type_synonym reg = nat

datatype instr'' = LDI int reg | LD vname reg | ADD reg reg

fun exec1'' :: "instr'' \<Rightarrow> state \<Rightarrow> (reg \<Rightarrow> int) \<Rightarrow> reg \<Rightarrow> int" where
  "exec1'' (LDI n r) s rs = rs(r := n)" |
  "exec1'' (LD v r) s rs = rs(r := s v)" |
  "exec1'' (ADD r1 r2) s rs = rs(r1 := (rs r1 + rs r2))"

fun exec'' :: "instr'' list \<Rightarrow> state \<Rightarrow> (reg \<Rightarrow> int) \<Rightarrow> reg \<Rightarrow> int" where
  "exec'' [] _ rs = rs" |
  "exec'' (i#is) s rs = exec'' is s (exec1'' i s rs)"

fun comp'' :: "aexp \<Rightarrow> reg \<Rightarrow> instr'' list" where
  "comp'' (N n) r = [LDI n r]" |
  "comp'' (V v) r = [LD v r]" |
  "comp'' (Plus e1 e2) r = (comp'' e1 r) @ (comp'' e2 (r+1)) @ [ADD r (r+1)]"

lemma exec''_append : "exec'' (is1 @ is2) s rs r=  exec'' is2 s (exec'' is1 s rs) r"
  by (induction is1 arbitrary: rs) auto

lemma comp''_under_r : "ri < r ==>  exec'' (comp'' e r) s rs ri = rs ri"
  by (induction e arbitrary: rs r) (auto simp add: exec''_append)

lemma "exec'' (comp'' a r ) s rs r = aval a s"
  by (induction a  arbitrary: rs r) (auto simp add: exec''_append comp''_under_r)

end
