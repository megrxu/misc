
def main : IO Unit := do
  IO.println "hello"
  IO.println "world"

def isGreaterThan0 (x : Nat) : IO Bool := do
  IO.println s!"value: {x}"
  return x > 0

def f (x : Nat) : IO Unit := do
  let c <- isGreaterThan0 x
  if c then
    IO.println s!"{x} is greater than 0"
  else
    pure ()

#eval f 10
-- value: 10
-- 10 is greater than 0