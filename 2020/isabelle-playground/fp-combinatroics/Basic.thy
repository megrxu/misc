theory Basic imports Main

begin

datatype nat' = Z | Suc nat'

fun plus :: "nat' \<Rightarrow> nat' \<Rightarrow> nat'" where
  "plus Z n = n" |
  "plus (Suc m) n = Suc (plus m n)"

lemma plus_z_m: "plus m Z = m"
  by (induction m; auto)

lemma plus_m_sn: "Suc (plus m n) = plus m (Suc n)"
  by (induction m; auto)

lemma "plus m n = plus n m"
  apply (induction m)
  by (simp add: plus_z_m) (simp add: plus_m_sn)

end