theory Functors
    imports Trees
begin

ML \<open>

(* structure ~ value
   signature ~ type
   functor ~ function. 
   This analogy is a starting point to understanding, but it fails 
   to convey the fullpotential of ML modules.*)

(* Queues *)

structure Queue1 =
    struct
    type 'a t = 'a list;
    exception E;

    val empty = []
    fun enq(q, x) = q @ [x];
    val null = List.null;
    val hd = List.hd;
    val deq = List.tl;
    end;

structure Queue2 =
    struct
    datatype 'a t = empty
                  | enq of 'a t * 'a;
    exception E;

    fun null (enq _) = false
      | null empty = true;
    fun hd (enq(empty, x)) = x
      | hd (enq(q, _)) = hd q
      | hd empty = raise E;
    fun deq (enq(empty, _)) = empty
      | deq (enq(q, x)) = enq(deq q, x)
      | deq empty = raise E;
    end;

structure Queue3 =
    struct
    datatype 'a t = Queue of ('a list * 'a list)
    exception E;

    val empty = Queue ([], []);
    fun norm (Queue([], tails)) = Queue(rev tails, [])
      | norm q = q;
    fun enq(Queue(heads, tails), x) = norm(Queue(heads, x::tails));
    fun null(Queue([], [])) = true
      | null _ = false;
    fun hd(Queue(x::_,_)) = x
      | hd(Queue([], _)) = raise E;
    fun deq(Queue(_::heads, tails)) = norm(Queue(heads, tails))
      | deq(Queue([], _)) = raise E;
    end;

signature QUEUE =
    sig
    type 'a t
    exception E
    val empty: 'a t
    val enq: 'a t * 'a -> 'a t
    val null: 'a t -> bool
    val hd: 'a t -> 'a
    val deq: 'a t -> 'a t
    end;

structure S1: QUEUE = Queue1;
structure S2: QUEUE = Queue2;
structure S3: QUEUE = Queue3;

structure AbsQueue1 :> QUEUE = Queue1;
structure AbsQueue2 :> QUEUE = Queue2;
structure AbsQueue3 :> QUEUE = Queue3;

AbsQueue1.empty;

abstype 'a queue = Empty
                 | Enq of 'a queue * 'a
    with
    val empty = Empty
    and enq = Enq

    fun qnull (Enq _) = false
      | qnull Empty = true;
    fun qhd (Enq(Empty, x)) = x
      | qhd (Enq(q, x)) = qhd q;
    fun deq (Enq(Epmty, x)) = Empty
      | deq (Enq(q, x)) = Enq(deq q, x);
    end;

functor TestQueue (Q: QUEUE) =
    struct
    fun formList l = List.foldl (fn (x, q) => Q.enq(q, x)) Q.empty l;
    fun toList q = if Q.null q then []
                   else Q.hd q :: toList (Q.deq q);
    end;

structure TestQ3 = TestQueue (Queue3);

TestQ3.formList [1];
TestQ3.toList (TestQ3.formList (1 upto 20));

signature ZSP =
    sig
    type t
    val zero : t
    val sum : t * t -> t
    val prod : t * t -> t
    end;

functor MatrixZSP (Z: ZSP) : ZSP =
    struct
    type t = Z.t list list;
    val zero = []

    fun sum (rowsA, []) = rowsA
      | sum ([], rowsB) = rowsB
      | sum (rowsA, rowsB) = ListPair.map (ListPair.map Z.sum)
                                          (rowsA, rowsB);
    fun dotprod pairs = List.foldl Z.sum Z.zero (ListPair.map Z.prod pairs);
    fun transp ([]::_) = []
      | transp rows = map hd rows :: transp (map tl rows);
    fun prod (rowsA, []) = []
      | prod (rowsA, rowsB) =
            let val colsB = transp rowsB
            in map (fn row => map (fn col => dotprod(row, col))
                                             colsB)
                    rowsA
            end;
    end;

structure IntZSP =
    struct
    type t = int;
    val zero = 0;
    fun sum (x, y) = x + y;
    fun prod (x, y) = x * y;
    end;

structure IntMatrix = MatrixZSP (IntZSP);
structure IntMatrixMatrix = MatrixZSP (IntMatrix); (* Matrix self is ZSP. *)

structure ComplexMatrix = MatrixZSP (Complex);

structure BoolZSP =
    struct
    type t = bool;
    val zero = false;
    fun sum(x, y) = x orelse y;
    fun prod(x, y) = x andalso y;
    end;

structure BoolMatrix = MatrixZSP (BoolZSP);


\<close>


end