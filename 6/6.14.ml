type intseq = Cons of int * (int -> intseq)
let rec nthseq n (Cons(x, f)) =
  if n = 1 then x
  else nthseq (n-1) (f x)

(* 6.5.2のis_prime*)
let is_prime x =
  let rec is_divisible_from_2_to n =
    (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n-1))
  in not (is_divisible_from_2_to (x-1))

let rec prime_seq x =
  if is_prime (x+1) then Cons(x+1, prime_seq) else prime_seq (x+1)

(* ① 割る数として，小さい数のほうから試していく *)
let is_prime_1 x =
  let rec is_divisible_from_2_to n =
    (n < x) && ((x mod n = 0) || is_divisible_from_2_to (n+1))
  in not (is_divisible_from_2_to 2)

let rec prime_seq_1 x =
  if is_prime_1 (x+1) then Cons(x+1, prime_seq_1) else prime_seq_1 (x+1)

(* ② 割る数の上限を ⌊√n⌋ にする *)
let is_prime_2 x =
  let rec is_divisible_from_2_to n =
    (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n-1))
  in not (is_divisible_from_2_to (int_of_float(floor(sqrt(float(x))))))

let rec prime_seq_2 x =
  if is_prime_2 (x+1) then Cons(x+1, prime_seq_2) else prime_seq_2 (x+1)

(* ③ 割る数として，n より小さい素数だけを試す *)
let rec is_prime_3 primes x =
  match primes with
    [] -> true
  | p :: rest -> 
    if p >= x then 
      true 
    else if x mod p = 0 then 
      false 
    else 
      is_prime_3 rest x

let rec prime_seq_3 primes x =
  if is_prime_3 primes (x+1) then Cons(x+1, prime_seq_3 (primes @ [x+1])) else prime_seq_3 primes (x+1)

(* ④ 割る数として，⌊√n⌋ 以下の素数だけを試す *)
let rec is_prime_4 primes x =
  match primes with
    [] -> true
  | p :: rest -> 
    if p > int_of_float(floor(sqrt(float(x)))) then 
      true 
    else if x mod p = 0 then 
      false 
    else 
      is_prime_4 rest x

let rec prime_seq_4 primes x =
  if is_prime_4 primes (x+1) then Cons(x+1, prime_seq_4 (primes @ [x+1])) else prime_seq_4 primes (x+1)
