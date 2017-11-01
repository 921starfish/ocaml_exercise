type intseq = Cons of int * (int -> intseq)

let rec nthseq n (Cons(x, f)) =
  if n = 1 then x
  else nthseq (n-1) (f x)

let rec fibonacci m n x = Cons(x+n, fibonacci (m+n) m)

let fib = fibonacci 0 1 0