theory Trees
    imports Lists
begin

ML \<open>
datatype person = King
                | Peer of string * string * int
                | Knight of string
                | Peasant of string;

King;
Peer("Earl", "Carlisle", 7);
Knight "Empty";

fun title King = "His Majesty the King"
  | title (Peer(deg, terr, _)) = "The" ^ deg ^ " of " ^ terr
  | title (Peasant name) = "Sir " ^ name;

fun sirs [] = []
  | sirs ((Knight s) :: ps) = s :: (sirs ps)
  | sirs (_ :: ps) = sirs ps;

(* Eex. 4.1 *)

local fun mat_to_int King = 4
        | mat_to_int (Peer _) = 3
        | mat_to_int (Knight _) = 2
        | mat_to_int (Peasant _) = 1
in fun superior (p1, p2) = mat_to_int p1 > mat_to_int p2 end;

superior (King, Knight "test");

SOME 1;

exception Failurebecause of string;
Failurebecause "Overflow";

exception Empty;

datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

fun size Lf = 0
  | size (Br(_, t1, t2)) = 1 + size t1 + size t2;

fun depth Lf = 0
  | depth (Br(_, t1, t2)) = 1 + Int.max(depth t1, depth t2);

fun comtree (k, n) =
    if n = 0 then Lf
             else Br (k, comtree(2*k, n-1),
                         comtree(2*k+1, n-1));

comtree(1,3);

fun reflect Lf = Lf
  | reflect (Br(v, t1, t2)) = Br(v, reflect t2, reflect t1);

fun compsame(_, 0) = Lf
  | compsame(x, n) =
        let fun replicate(t, 0) = t
              | replicate(t, n) = replicate(Br(x, t, t), n-1);
        in replicate(Lf, n) end;

compsame(1,3);

(* Propositional Logic *)

datatype prop = Atom of string
              | Neg of prop
              | Conj of prop * prop
              | Disj of prop * prop;

fun implies(p, q) = Disj(Neg p, q);

fun show (Atom a) = a
  | show (Neg p) = "(" ^ show p ^ ")"
  | show (Conj (p, q)) = "(" ^ show p ^ " & " ^ show q ^ ")"
  | show (Disj (p, q)) = "(" ^ show p ^ " | " ^ show q ^ ")";

(* NNF! Negation normal form. *)

fun nnf (Atom a) = Atom a
  | nnf (Neg (Atom a)) = Neg (Atom a)
  | nnf (Neg (Neg (p))) = nnf p
  | nnf (Neg (Conj(p, q))) = nnf (Disj(Neg p, Neg q))
  | nnf (Neg (Disj(p, q))) = nnf (Conj(Neg p, Neg q))
  | nnf (Conj(p, q)) = Conj (nnf p, nnf q)
  | nnf (Disj(p, q)) = Disj(nnf p, nnf q);

fun nnfpos (Atom a) = Atom a
  | nnfpos (Neg p) = nnfneg p
  | nnfpos (Conj(p, q)) = Conj (nnfneg p, nnfneg q)
  | nnfpos (Disj(p, q)) = Disj (nnfneg p, nnfneg q)
and nnfneg (Atom a) = Neg (Atom a)
  | nnfneg (Neg p) = nnfpos p
  | nnfneg (Conj(p, q)) = Disj(nnfneg p, nnfneg q)
  | nnfneg (Disj(p, q)) = Conj(nnfneg p, nnfneg q);

(* CNF! Conjunctive normal form. *)

fun distrib (p, Conj(q, r)) = Conj (distrib (p, q), distrib (p, r))
  | distrib (Conj(q, r), p) = Conj (distrib (p, q), distrib (p, r))
  | distrib (p, q) = Disj (p, q)

fun cnf (Conj(p, q)) = Conj (cnf p, cnf q)
  | cnf (Disj(p, q)) = distrib (cnf p, cnf q)
  | cnf p = p;

fun positives (Atom a) = [a]
  | positives (Neg (Atom _)) = []
  | positives (Disj(p, q)) = positives p @ positives q

fun negatives (Atom a) = []
  | negatives (Neg (Atom a)) = [a]
  | negatives (Disj(p, q)) = negatives p @ negatives q;

\<close>

end