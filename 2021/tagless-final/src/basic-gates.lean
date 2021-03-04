namespace basic_gates

class SYM (r : Type) :=
  (lit : bool -> r)
  (neg : r -> r)
  (and : r -> r -> r)
  (or  : r -> r -> r)

prefix `¬` := SYM.neg
infix `&&` := SYM.and
infix `||` := SYM.or

open SYM
def ex1 {r : Type} (i: SYM r) : r := lit true && (¬ (lit false))

class SYMW (r : Type) extends SYM r :=
  (wire_x  : r )
  (wire_y  : r )
  (wire_z  : r )

open SYMW
def ex2 {r : Type} (i: SYMW r) : r := lit true && ¬ wire_x

def xor {r: Type} [SYM r] (x y : r):  r := (x || y) && (¬ (x && y))

-- Simple Adder

def exadd2 {r: Type} (i: SYMW r) := xor i.wire_x i.wire_y
def exadd3 {r: Type} (i: SYMW r) := xor (xor i.wire_x i.wire_y) i.wire_z
def exadd_xy1 {r: Type} (i: SYMW r) := xor (xor i.wire_x i.wire_y) (lit true)

-- An implementation

def R: SYM bool := {
  lit := id,
  neg := bnot,
  and := band,
  or  := bor
}

#eval (ex1 R : bool)

inductive symd : Type
| Lit : bool -> symd
| Neg : symd -> symd
| And : symd -> symd -> symd
| Or  : symd -> symd -> symd

open symd
def exd := Neg (And (Lit tt) (Neg (Lit ff)))

def evalR : symd -> bool
  | (Lit x) := x
  | (Neg e) := bnot (evalR e)
  | (And e1 e2) := band (evalR e1) (evalR e2)
  | (Or  e1 e2) := bor (evalR e1) (evalR e2)

#eval evalR exd -- Now it evaluates!

def RW: SYMW bool :=  {
  wire_x := true,
  wire_y := false,
  wire_z := true,
  ..R
}

def RWV (x y z: bool) : SYMW bool := {
  wire_x := x,
  wire_y := y,
  wire_z := z,
  ..R
}

#eval (exadd2 RW : bool)
#eval (exadd2 (RWV tt ff ff) : bool)
#eval (exadd3 (RWV tt ff ff) : bool)
#eval (ex1 R: bool)

-- To show
local infix `++` := string.append
def S0: SYM string := 
let paren (s : string) := "(" ++ s ++ ")" in
{
  lit := λ x, match x with
              | tt := "tt"
              | ff := "ff"
              end,
  neg := λ x, "¬" ++ x,
  and := λ x y, paren (x ++ " && " ++ y),
  or  := λ x y, paren (x ++ " || " ++ y),
}
def SW0 : SYMW string := {
  wire_x := "X",
  wire_y := "Y",
  wire_z := "Z",
  ..S0
}

#eval (ex1 S0)
#eval (exadd2 SW0)

def observe (x : int -> string) := x 0
def S: SYM (int -> string) := 
let paren (y: bool) (s : string): string := 
if y then "(" ++ s ++ ")" else s
in
{
  lit := λ x, match x with
              | tt := λ _, "tt"
              | ff := λ _, "ff"
              end,
  neg := λ x, λ p, paren (p>10) ("¬" ++ x 11),
  and := λ x y, λ p, paren (p>4) (x 4 ++ " && " ++ y 4),
  or  := λ x y, λ p, paren (p>3) (x 3 ++ " || " ++ y 3),
}

def SW: SYMW (int -> string) := {
  wire_x := λ _, "X",
  wire_y := λ _, "Y",
  wire_z := λ _, "Z",
  ..S
}

#eval observe (exadd2 SW)

end basic_gates
