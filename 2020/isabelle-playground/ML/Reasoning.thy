theory Reasoning
    imports Trees
begin

ML \<open>

datatype evenodd = Even | Odd;

fun test 0 = Even
  | test n = (case test (n - 1) of
                    Even => Odd
                  | Odd => Even);

fun half 0 = (Even, 0)
  | half n = (case half (n - 1) of
                    (Even, m) => (Odd, m)
                  | (Odd, m) => (Even, m+1));

(* d != 0 *)
local fun iter (res, n, d) = if n < d then (res, n) else iter (res + 1, n - d, d)
in
fun quotient n d = iter (0, n, d) end;

quotient 100 8;
quotient 78 2;

(* Structural induction on lists and trees. Omitted. *)

\<close>

end