theory Equality
  imports Main

begin

(* 4.1 *)

datatype 'a tree = Tip | Node "'a tree" 'a "'a tree"

fun set :: "'a tree \<Rightarrow> 'a set" where
  "set Tip = {}" |
  "set (Node ln v rn) = { v } \<union> (set ln) \<union> (set rn)"

fun ord :: "int tree \<Rightarrow> bool" where
  "ord Tip = True" |
  "ord (Node ln v rn) = ((\<forall> lv \<in> set ln. v > lv) \<and> (\<forall> rv \<in> set rn. rv > v) \<and> ord ln \<and> ord rn)"

fun ins :: "int  \<Rightarrow> int tree \<Rightarrow> int tree" where
  "ins n Tip = (Node Tip n Tip)" |
  "ins n (Node ln v rn) = (if (v = n) then (Node ln v rn) else (
                           if (v > n) then (Node (ins n ln) v rn) else
                                           (Node ln v (ins n rn))))"

lemma in_set: "set (ins x t ) = { x } \<union> set t"
  by (induction t) (auto)

theorem "ord t \<Longrightarrow> ord (ins i t)"
  by (induction t) (auto simp add: in_set)


(* 4.2 *)

inductive palindrome :: "'a list \<Rightarrow> bool" where
  palNil: "palindrome []"  |
  palSin: "palindrome [_]" |
  palOth: "palindrome xs \<Longrightarrow> palindrome (a # xs @ [a])"

theorem rev_pal: "palindrome l \<Longrightarrow> rev l = l"
  by (induction rule:palindrome.induct) (auto)

(* 4.3 *)
inductive star :: "( 'a \<Rightarrow> 'a \<Rightarrow> bool) \<Rightarrow> 'a \<Rightarrow> 'a \<Rightarrow> bool" for r where
refl: "star r x x" |
step: "r x y \<Longrightarrow> star r y z \<Longrightarrow> star r x z"

inductive star' :: "( 'a \<Rightarrow> 'a \<Rightarrow> bool) \<Rightarrow> 'a \<Rightarrow> 'a \<Rightarrow> bool" for r where
refl' : "star' r x x" |
step' : "star' r x y \<Longrightarrow> r y z \<Longrightarrow> star' r x z"

lemma [simp]: "star r x y \<Longrightarrow> (star' r x y \<or> (\<exists> m. ((star r x m) \<and> (star r m y))))"
  by (meson star.refl)

lemma [simp]: "star' r x y \<Longrightarrow> (star r x y \<or> (\<exists> m. ((star' r x m) \<and> (star' r m y))))"
  by (meson refl')

lemma star'_prepend:
  "\<lbrakk>star' r y z ; r x y\<rbrakk> \<Longrightarrow> star' r x z"
  by (induction rule: star'.induct) (auto intro: star'.intros)

lemma star_append: "\<lbrakk> star r x y; r y z \<rbrakk> \<Longrightarrow> star r x z"
  by (induct rule: star.induct) (auto intro: star.intros)

lemma "star r x y = star' r x y"
proof
  assume "star r x y"
  thus "star' r x y"
    by induction (auto intro: star'.intros star'_prepend)
next
  assume "star' r x y"
  thus "star r x y"
    by induction (auto intro: star.intros star_append)
qed

(* 4.4 *)
inductive iter :: "('a \<Rightarrow> 'a \<Rightarrow> bool) \<Rightarrow> nat \<Rightarrow> 'a \<Rightarrow> 'a \<Rightarrow> bool" for r where
  iter_self: "iter r 0 x x" |
  iter_rule: "r x x' \<Longrightarrow> iter r (n-1) x' y \<Longrightarrow> iter r n x y"

(* 4.5 *)
datatype alpha = a | b

inductive S :: "alpha list \<Rightarrow> bool" where
  eptS: "S []" |
  symS: "S w \<Longrightarrow> S (a # w @ [b])" |
  conS: "S w1 \<Longrightarrow> S w2 \<Longrightarrow> S (w1 @ w2)"

inductive T :: "alpha list \<Rightarrow> bool" where
  eptT: "T []" |
  sftT: "T w1 \<Longrightarrow> T w2 \<Longrightarrow> T (w1 @ [a] @ w2 @ [b])"

lemma [simp]: "T w \<Longrightarrow> T (a # w @ [b])"
  using eptT sftT by force


lemma [simp]: "T w \<Longrightarrow> w = [] \<or> (\<exists> w1 w2. w = w1 @ [a] @ w2 @ [b])"
  using T.cases by auto

lemma [simp]: "T w \<Longrightarrow> w = [] \<or> (\<exists> w'. T w' \<and> w = [a] @ w' @ [b])"
proof -
  assume "T w"
  hence inv: "(w = []) \<or> (\<exists> w1 w2. w = w1 @ [a] @ w2 @ [b] \<and> T w1 \<and> T w2)" using T.cases by auto
  thus "?thesis"
  proof (induct w)
    case Nil
    then show ?case by simp
  next
    case (Cons a w)
    assume "(w = []) \<or> (\<exists>w1 w2. w = w1 @ [a] @ w2 @ [b] \<and> T w1 \<and> T w2)"
    then have ""
  qed

    
  qed
qed

lemma [simp]: "T w1 \<Longrightarrow> T w2 \<Longrightarrow> T (w1 @ w2)"
proof -
  assume "T w1"
  then show "?thesis" by blast
qed

lemma "T w = S w"
proof
  assume "T w"
  thus "S w" by (induct) (auto intro: S.intros)
next
  assume "S w"
  thus "T w" by induct (auto intro: T.intros)
qed

end