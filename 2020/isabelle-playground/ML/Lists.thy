theory Lists
    imports ML_Playground
begin

text "Simple lists"

ML \<open>

infix upto_;
fun op upto_ (m, n) =
    if m > n then [] else m :: op upto_ (m+1, n);
2 upto_ 5;

fun prod [] = 1
  | prod (n::ns) = n * (prod ns);

fun maxl [m] : int = m
  | maxl (m::n::ns) = if m > n then maxl(m::ns)
                               else maxl(n::ns);

fun factl (n) = prod (op upto (1, n));

(* In Isabelle/ML, explode is under the String signature. *)
String.explode "Explode";
String.implode (String.explode "Explode");
String.concat ["Explode"];

hd [0, 1, 2];

(* ex 3.1 *)
fun maxl n = if (null (tl n)) then hd n
                              else if (hd n) > (hd (tl n)) then maxl ((hd n)::(tl (tl n)))
                                                           else maxl ((hd (tl n))::(tl (tl n)));

maxl [1, 8, 2, 3, 10, 9, 0];

local
    fun addlen (n, []) = n
      | addlen (n, _::l) = addlen(n + 1, l)
in
    fun length l = addlen (0, l)
end;

fun rtake ([], _, taken) = taken
  | rtake (x::xs, i, taken) =
          if i > 0 then rtake (xs, i-1, x::taken)
                        else taken;

rtake ([9, 8, 7, 6], 3, []);

fun unzip [] = ([], [])
  | unzip ((x, y)::pairs) = 
            let val (xs, ys) = unzip pairs
            in (x :: xs, y :: ys) end;

unzip [(0, 0), (1, 0)];

(* 3.8 Binary Arithmetic *)

fun bincarry (0, ps) = ps
  | bincarry (1, []) = [1]
  | bincarry (1, p::ps) = (1 - p) :: bincarry (p, ps);

fun binsum (c, [], qs) = bincarry (c , qs)
  | binsum (c, ps, []) = bincarry (c, ps)
  | binsum (c, p :: ps, q :: qs) = 
           ((c + p + q) mod 2) :: binsum ((c + p + q) div 2, ps, qs);

binsum (0, [1,1,0,1], [0,1,1,1,1]);

fun binprod ([], _) = []
  | binprod (0::ps, qs) = 0::binprod(ps, qs)
  | binprod (1::ps, qs) = binsum (0, qs, 0::binprod(ps, qs));

binprod([1,1,0,1], [0,1,1,1,1]);

(* some exercises and sections are very similar to SICP, and will be omitted. *)

(* 3.12 *)
fun squares r =
    let fun between (x, y) = 
            let val diff = r - x * x
            fun above y = 
                if y > x then []
                else if y * y < diff then above (y + 1)
                else if y * y = diff then (x, y)::between(x-1, y+1)
                else between (x - 1, y)
            in above y end;
        val firstx = floor(Math.sqrt (real r))
    in between (firstx, 0) end;

squares 50;

op +;

(* there is no equality for real in Poly? *)
infix nearly;
fun a nearly b = (abs (a - b)) < 1.0E~10;

structure Poly =
    struct
    type t = (int*real) list
    val zero = [] : t
    fun sum ([], us) = us : t
      | sum (ts, []) = ts
      | sum ((m,a)::ts, (n,b)::us) =
            if m > n then (m,a)::sum(ts, (n, b)::us)
            else if n>m then (n,b)::sum(us, (m,a)::ts)
            else if (a+b) nearly 0.0 then sum (ts, us)
                              else (m, a+b)::sum (ts, us);
    fun termprod ((m, a), []) = [] : t
      | termprod ((m, a), (n, b)::ts) =
            (m+n, a*b) :: termprod((m, a), ts);
    fun nprod ([], us) : t = []
      | nprod ((m,a)::ts, us) = sum ((termprod ((m, a), us)),
                                    nprod (ts, us));
    fun prod ([], us) = [] : t
      | prod ([(m,a)], us) = termprod ((m, a), us)
      | prod (ts, us) =
             let val k = length ts div 2
             in sum (prod (List.take (ts, k), us),
                     prod (List.drop (ts, k), us))
             end;
    end;

\<close>
end