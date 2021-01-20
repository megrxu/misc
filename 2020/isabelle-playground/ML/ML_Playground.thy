theory "ML_Playground"
    imports Pure

begin

text "Some basic things again.."

ML \<open>
fun length [] = 0
  | length (_::xs) = 1 + length xs;

(* Values. *)
2+2;
0.9 : real;
Math.sqrt 2.0;
val seconds = 60 : int;
val pi = 3.1415926 : real;
"Hello, " ^ "world";

fun area (r) = pi * r * r;

area(2.0);
area 1.0; (* the parentheses are optional *)

(* Another way of function declaration. *)
(* Redeclaring a function cannot damage the system, the library or your program. *)
fun area r = pi * r * r;

fun square (x : real) = x * x;

val a = 0 - 1;
val b = ~1.0;

(* Ex. 2.5 *)
(* Is there anything like `multiple pattern matching` in one case in Isabelle/ML? *)
fun valid_date (d : int) (m : string) = case (d, m) of
       (d, "Jan") => (d >= 1) andalso (d <= 31)
    |  (d, "Feb") => (d >= 1) andalso (d <= 28)
    |  (d, "Mar") => (d >= 1) andalso (d <= 31)
    |  (d, "Apr") => (d >= 1) andalso (d <= 30)
    |  (d, "May") => (d >= 1) andalso (d <= 31)
    |  (d, "Jun") => (d >= 1) andalso (d <= 30)
    |  (d, "Jul") => (d >= 1) andalso (d <= 31)
    |  (d, "Aug") => (d >= 1) andalso (d <= 31)
    |  (d, "Sep") => (d >= 1) andalso (d <= 30)
    |  (d, "Oct") => (d >= 1) andalso (d <= 31)
    |  (d, "Nov") => (d >= 1) andalso (d <= 30)
    |  (d, "Dec") => (d >= 1) andalso (d <= 31)
    |  _ => false;

valid_date 29 "Feb";
valid_date 30 "Mar";

type vec = real * real;
fun lengthvec (x: real, y : real) = Math.sqrt (square x + square y);
fun negvec (x: real, y : real) = (~x, ~y);
fun addvec ((x_1 : real, y_1 : real), (x_2 : real, y_2 : real)) = (x_1 + x_2, y_1 + y_2)
fun subvec (v1, v2) = addvec (v1, negvec v2);

(* A higher-order function *)
fun compose f g x = f (g x);
(* A function can be defined in this `val` way. *)
val distance = compose lengthvec subvec;
distance ((0.0, 0.0), (1.0, ~1.0));

(* Or in a lambda way. *)
val rec factorial = fn 0 => 1 | n => n * factorial(n - 1);
factorial 5;

(* Record type (like struct) *)

type king = {
  name : string,
  born : int,
  crowned : int,
  quote : string};

fun get_name (k: king) = #name k;

(* Infix *)
(* syntax: infix `precedence` `operator` *)
infix xor;
fun (p xor q) = (p orelse q) andalso not (p andalso q);

infix ++;
fun (v1 ++ v2) = addvec (v1, v2);
(0.0, 0.0) ++ (1.0, ~1.0);

(* call-by-value and call-by-need *)

(* `if then else` is not a simple function that call-on-value. See the `badf` example*)
fun cond(p, p1, p2) = if p then p1 else p2;
fun badf n = cond (n=0, 1, n*badf(n-1));
(* badf(0) runs forever due to call-by-value. *)

val test = true orelse (badf(0) = 1); (* there is short-circuit evaluation in SML. *)

fun even n = (n mod 2 = 0);
fun powoftwo n = (n=1) orelse
                 (even(n) andalso powoftwo (n div 2));
powoftwo 128;
powoftwo 48;
(* `powoftwo` is also iterative function. *)

(* call by need or lazy-evaluation *)
fun zero(p : 'a) = 0;
(* zero(badf(0)); still runs forever. *)
(* No wasteful computation, while the graph manipulations are expensive. *)
(* Data structures can be evaluated partially. *)
(* `zero(E)` can be evaluated even if E fails to terminate since there is no need to evaluate E. *)

(* Recursive functions *)
fun gcd(m, n) =
    if m = 0 then n
      else gcd (n mod m, m);

fun power(x, k) : real =
    if k = 1 then x
       else if k mod 2 = 0 then power (x * x, k div 2)
                                else x * power(x*x, k div 2);

fun itfib(n, prev, curr) : int =
    if n=1 then curr
    else itfib(n-1, curr, prev+curr);
fun fib n = itfib(n, 0, 1);

(* 2.16 *)
fun increase(k, n) = if (k + 1) * (k + 1) > n then k else k + 1;
fun introot n = 
    case n of
      0 => 0
    | _ => increase (2 * introot ( n div 4), n);

introot 100000000000000000;
square (real (introot 100000000000000000));

(* local declarations *)
(* let D in E end *)
(* local D in D end *)
fun fraction (n, d) =
    let val com = gcd(n, d)
    in (n div com, d div com) end;

fun findroot (a, x, acc) = 
    let val nextx = (a / x + x) / 2.0
    in if abs (x - nextx) < acc * x
       then nextx else findroot(a, nextx, acc)
    end;

fun sqroot a = findroot (a, 1.0, 1.0E~10);

sqroot 100.0;

fun sqroot a =
    let val acc = 1.0E~10
        fun findroot x =
            let val nextx = (a / x + x) / 2.0;
            in if abs ( x - nextx) < acc * x
               then nextx else findroot nextx
            end
    in findroot 1.0 end;

sqroot 100.0;

(* Mutually recursive functions. *)

fun pos d = neg(d - 2.0) + 1.0/d
and neg d = if d > 0.0 then pos(d-2.0) - 1.0/d
                else 0.0;

fun min x y = if x > y then y else x;
fun max x y = if x > y then x else y;

val (min, max) = (max, min);
max 1 0;
min 1 0;

(* ex 2.23 *)
fun p n = 1 + sum_p (n - 1)
and sum_p 0 = 0
  | sum_p n = sum_p (n - 1) + p n;

p 1;
p 3 = round(Math.pow (2.0, 2.0));

(* signatures *)
signature ARITH = 
  sig
  type t
  val zero : t
  val sum : t * t -> t
  val diff : t * t -> t
  val prod : t * t -> t
  val quo : t * t -> t
  end;

(* structures *)
structure Complex : ARITH =
  struct
  type t = real * real;
  val zero = (0.0, 0.0) ;
  fun sum ((x, y), (x', y')) = (x + x', y + y') : t;
  fun diff ((x, y), (x', y')) = (x -x', y - y') : t;
  fun prod ((x, y), (x', y')) = (x * x' - y * y', x * y' + x' * y) : t;
  fun recip (x,y) = 
                    let val t = x*x + y*y 
                    in (x/t, ~y/t) end
  fun quo (z, z') = prod (z, recip z') ;
  end;

val i = (0.0, 0.0);
val a = (0.3, 1.0);

val b = Complex.sum(a, i);
fst b;

(* Polymorphic function declarations *)

fun pairself x = (x, x);

(* the `id` in SML *)
I "a";
\<close>

end