theory Isar
  imports Main

begin

(* an example *)

lemma "\<not> surj(f :: 'a \<Rightarrow> 'a set)"
proof
  assume 0: "surj f"
  from 0 have 1: "\<forall> A. \<exists> a. A = f a" by (simp add: surj_def)
  from 1 have 2: "\<exists> a. {x. x \<notin> f x} = f a" by blast
  from 2 show "False" by blast
qed

lemma "\<not> surj(f :: 'a \<Rightarrow> 'a set)"
proof
  assume 0: "surj f"
  from this have "\<exists> a. {x. x \<notin> f x} = f a" by (simp add: surj_def)
  from this show "False" by blast
qed

lemma "\<not> surj(f :: 'a \<Rightarrow> 'a set)"
proof
  assume 0: "surj f"
  hence "\<exists> a. {x. x \<notin> f x} = f a" by (simp add: surj_def)
  thus "False" by blast
qed

lemma
  fixes f :: "'a \<Rightarrow> 'a set"
  assumes s: "surj f"
  shows "False"
proof -
  have "\<exists> a. {x. x \<notin> f x} = f a" using s
    by (auto simp: surj_def)
  thus "False" by blast
qed

lemma fixes a b :: int assumes "b dvd (a+b)" shows "b dvd a"
proof -
  have "\<exists> k . a = b * k" if asm: "a + b = b * k" for k
  proof
    show "a = b * (k - 1)" using asm by(simp add: algebra_simps)
  qed
then show ?thesis using assms by(auto simp add: dvd_def )
qed

(* 5.1 *)
lemma assumes T : "\<forall> x y. T x y \<or> T y x"
  and A: "\<forall> x y. A x y \<and> A y x \<longrightarrow> x = y"
  and TA: "\<forall> x y. T x y \<longrightarrow> A x y" and AX: "A x y"
  shows "T x y"
proof (rule ccontr)
  assume "~(T x y)"
  moreover hence "T y x" using T by auto
  moreover hence "A y x" using TA by simp
  moreover hence "x = y" using AX A by simp
  moreover hence "T x y" using T by auto
  ultimately show "False"  using assms by auto
qed

(* 5.2 *)
lemma "\<exists> ys zs. xs = ys @ zs \<and> (length ys = length zs \<or> length ys = length zs + 1)"
proof -
  consider (even) "even (length xs)" | (odd) "odd (length xs)" by auto
  then show ?thesis
  proof (cases)
    case even
    assume "even (length xs)"
    let ?ys' = "take (length xs div 2) xs"
    let ?zs' = "drop (length xs div 2) xs"
    have 0: "length ?ys' = length ?zs'" using even by auto 
    have 1: "xs = ?ys' @ ?zs'" by simp
    thus ?thesis using 0 1 by blast
  next
    case odd
    assume "odd (length xs)"
    let ?ys' = "take (length xs div 2 + 1) xs"
    let ?zs' = "drop (length xs div 2 + 1) xs"
    have 0: "length ?ys' = length ?zs' + 1"
      (* try here *)
      by (smt Suc_eq_plus1 add_Suc_right add_diff_cancel_left' drop_drop length_drop length_rev mult_2 odd odd_two_times_div_two_nat odd_two_times_div_two_succ rev_take)
    have 1: "xs = ?ys' @ ?zs'" by simp
    thus ?thesis using 0 1 by blast
  qed
qed

(* 5.6 *)
fun elems :: "'a list \<Rightarrow> 'a set" where
  "elems [] = {}" |
  "elems (x # xs) = {x} \<union> elems xs"

lemma "x \<in> elems xs \<Longrightarrow> \<exists> ys zs. xs = ys @ x # zs \<and> x \<notin> elems ys"
proof (induct xs)
  case Nil
  then show ?case by auto
next
  case (Cons a xs)
  consider (head) "x = a" | (tail) "x \<in> elems xs" using Cons.prems by auto
  then show ?case
  proof (cases)
    case head
    then show ?thesis by fastforce
  next
    case tail
    then show ?thesis by (metis Cons.hyps Cons_eq_append_conv Un_iff elems.simps(1) elems.simps(2) empty_iff singletonD)
  qed
qed

end