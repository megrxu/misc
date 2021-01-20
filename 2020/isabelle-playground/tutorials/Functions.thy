theory Functions imports Main

begin

fun fib:: "nat \<Rightarrow> nat" where
  "fib 0 = 1"
| "fib (Suc 0) = 1"
| "fib (Suc (Suc n)) = fib (Suc n) + fib n"

function sum :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "sum i N = (if i > N then 0 else i + sum (Suc i) N)"
  by pat_completeness auto
  termination sum
    apply (relation "measure (\<lambda>(i, N). N + 1 - i)")
    apply auto
    done

function foo :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
  "foo i N = (if i > N
              then (if N = 0 then 0 else foo 0 (N - 1))
              else i + foo (Suc i) N)"
  by pat_completeness auto
  termination foo
    apply (relation "measures [(\<lambda>(i, N). N), (\<lambda>(i, N). N + 1 - i)]")
    apply auto
    done

function even :: "nat \<Rightarrow> bool" and odd :: "nat \<Rightarrow> bool" where
  "even 0 = True"
| "odd 0 = False"
| "even (Suc n) = odd n"
| "odd (Suc n) = even n"
  by pat_completeness auto
termination
  by (relation "measure (\<lambda>x. case x of Inl n \<Rightarrow> n | Inr n \<Rightarrow> n)") auto

thm even.elims
thm odd.cases

end