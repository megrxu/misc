(* `ref` is not allowed in Isabelle/ML by default. So we use Poly/ML instead. *)

fun impFact n =
    let val resultp = ref 1
        and ip = ref 0
    in while !ip < n do (ip := !ip + 1;
                        resultp := !resultp * !ip);
        !resultp
    end;

(* Skipped for now.. *)