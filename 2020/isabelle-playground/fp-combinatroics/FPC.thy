theory FPC imports Main "HOL-Lattice.Orders"

begin

instantiation set :: (type) partial_order
begin

definition leq_set_def: "x \<sqsubseteq> y = subset_eq x y"

instance
  by intro_classes (simp_all add: leq_set_def)
end

definition chain :: "'a::partial_order set \<Rightarrow> 'a::partial_order set set" where
  "chain ori = {s. s \<sqsubseteq> ori \<and> (\<forall> x \<in> s. \<forall> y \<in> s. x \<sqsubseteq> y \<or> y \<sqsubseteq> x)}"

definition antichain :: "'a::partial_order set \<Rightarrow> 'a::partial_order set set" where
  "antichain ori = {s. s \<sqsubseteq> ori \<and> \<not>(\<exists> x \<in> s. \<exists> y \<in> s. x \<sqsubseteq> y \<or> y \<sqsubseteq> x)}"

definition cover :: "'a::partial_order set \<Rightarrow> 'a::partial_order set set set" where
  "cover ori = {s. \<forall> x \<in> ori. \<exists> e \<in> s. x \<in> e}"

end