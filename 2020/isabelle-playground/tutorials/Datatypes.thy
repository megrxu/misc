theory Datatypes imports Main

begin

datatype trool = Truue | Faalse | Perhaaps

datatype 'a option = None | Some 'a

datatype ('a, 'b, 'c) triple = Triple 'a 'b 'c

datatype nat = Zero | Succ nat

datatype even_nat = Even_Zero | Even_Succ odd_nat and odd_nat = Odd_Succ even_nat

datatype 'a btree = BNode 'a "'a btree option" "'a btree option"

value "BNode nat (Some (BNode nat None None)) None"

datatype ('a, 'b) fun_copy = Fun "'a \<Rightarrow> 'b"

datatype_compat even_nat odd_nat

primrec (nonexhaustive) bool_of_trool :: "trool \<Rightarrow> bool" where
  "bool_of_trool Faalse \<longleftrightarrow> False" |
  "bool_of_trool Truue \<longleftrightarrow> True"

primrec replicate :: "nat \<Rightarrow> 'a \<Rightarrow> 'a list" where
  "replicate Zero _ = []" |
  "replicate (Succ n) x = x # replicate n x"

codatatype (lset: 'a) llist = 
  lnull: LNil |
  LCons (lhd: 'a) (ltl: "'a llist")
for 
  map : lmap
  rel: llist_all2
  pred: llist_all
where
  "ltl LNil = LNil"

typedef ('d, 'a) fn = "UNIV :: ('d \<Rightarrow> 'a) set"
  by simp

setup_lifting type_definition_fn

lift_definition map_fn :: "('a \<Rightarrow> 'b) \<Rightarrow> ('d, 'a) fn \<Rightarrow> ('d, 'b) fn" is "(o)" .
lift_definition set_fn :: "('d, 'a) fn \<Rightarrow> 'a set" is range .
lift_definition pred_fn :: "('a \<Rightarrow> bool) \<Rightarrow> ('d, 'a) fn \<Rightarrow> bool" is "pred_fun (\<lambda>_. True)" .
lift_definition rel_fn :: "('a \<Rightarrow> 'b \<Rightarrow> bool) \<Rightarrow> ('d, 'a) fn \<Rightarrow> ('d, 'b) fn \<Rightarrow> bool" is "rel_fun (=)".

end