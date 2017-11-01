type ('a, 'b) sum = Left of 'a | Right of 'b
(* ① f1: 'a * ('b, 'c) sum -> ('a * 'b, 'a * 'c) sum *)
let f1 x = 
  match x with
    (y,Left z) -> Left (y,z)
  | (y,Right z) -> Right (y,z)

(* ② f2: ('a * 'b, 'a * 'c) sum -> 'a * ('b, 'c) sum *)
let f2 x =
  match x with
    Left (y, z) -> (y, Left z)
  | Right (y, z) -> (y, Right z)

(* ③ f3: ('a, 'b) sum * ('c, 'd) sum -> (('a * 'c, 'b * 'c) sum, ('a * 'd, 'b * 'd) sum) sum *)
let f3 x =
  match x with
    (Left y, Left z) -> Left(Left(y, z))
  | (Left y, Right z) -> Right(Left(y, z))
  | (Right y, Right z) -> Right(Right(y, z))
  | (Right y, Left z) -> Left(Right(y, z))

(* ④ f4: (('a * 'c, 'b * 'c) sum, ('a * 'd, 'b * 'd) sum) sum -> ('a, 'b) sum * ('c, 'd) sum *)
let f4 x =
  match x with
    Left(Left(y, z)) -> (Left y, Left z)
  | Right(Right(y, z)) -> (Right y, Right z)
  | Right(Left(y, z)) -> (Left y, Right z)
  | Left(Right(y, z)) -> (Right y, Left z)

(* ⑤ f5: ('a -> 'b) * ('c -> 'b) -> ('a, 'c) sum -> 'b *)
let f5 (f,g) x =
  match x with
    Left a -> f a
  | Right c -> g c

(* ⑥ f6: (('a, 'b) sum -> 'c) -> ('a -> 'c) * ('b -> 'c) *)
let f6 f = 
  ((fun a -> f (Left a)), (fun b -> f (Right b)))

(* ⑦ f7: ('a -> 'b, 'a -> 'c) sum -> 'a -> ('b,'c) sum *)
let f7 x a=
  match x with
    Left f -> Left(f a)
  | Right g -> Right(g a)