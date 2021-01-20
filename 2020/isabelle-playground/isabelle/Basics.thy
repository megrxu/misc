theory Basics
  imports Main
begin

(* Some definitions and functions that will overwrite std libs in chapter 2 *)

datatype bool = True | False

fun conj :: "bool \<Rightarrow> bool \<Rightarrow> bool" where
  "conj True True = True" |
  "conj _ _ = False"

datatype nat = Z | Suc nat

fun add :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "add Z n = n" |
  "add (Suc m) n = Suc (add m n)"

lemma add_02: "add m Z = m"
  by (induction m) auto

thm add_02

datatype 'a list = Nil | Cons 'a "'a list"

fun app :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
  "app Nil ys = ys" | 
  "app (Cons x xs) ys = Cons x (app xs ys)"

fun rev :: "'a list \<Rightarrow> 'a list" where
  "rev Nil = Nil" |
  "rev (Cons x xs) = app (rev xs) (Cons x Nil)"

value "rev (Cons True (Cons False Nil))"

lemma app_nil [simp]: "app xs Nil = xs"
  by (induction xs) auto

lemma app_assoc [simp]: "app (app xs ys) zs = app xs (app ys zs)"
  by (induction xs) auto

lemma rev_app [simp]: "rev (app xs ys) = app (rev ys) (rev xs)"
  by (induction xs) auto

fun map :: "('a \<Rightarrow> 'b) \<Rightarrow> 'a list \<Rightarrow> 'b list" where
  "map f Nil = Nil" |
  "map f (Cons x xs) = Cons (f x) (map f xs)"

fun hd :: "'a list \<Rightarrow> 'a" where "hd (Cons x xs) = x"

fun tl :: "'a list \<Rightarrow> 'a list" where 
  "tl (Cons x xs) = xs" |
  "tl _ = Nil"

end
