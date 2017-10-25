module type QUEUE = sig
  type 'a t
  exception Empty
  val empty: 'a t
  val add: 'a t -> 'a -> 'a t
  val take: 'a t -> 'a * 'a t
  val peek: 'a t -> 'a
end;;

module Queue1 : QUEUE =
struct
  type 'a t = 'a list
  (*ここから*)
  exception Empty
  let empty = []
  let add queue a = queue @ [a]
  let take queue =
    match queue with
      [] -> raise Empty
    | a::rest -> (a, rest)
  (*ここまで*)
  let peek = function [] -> raise Empty | x :: rest -> x
end

module Queue2 : QUEUE =
struct
  type 'a t = Queue of ('a list * 'a list)
  (*ここから*)
  exception Empty
  let empty = Queue([],[])
  let add queue a = 
    match queue with
      Queue([],[]) -> Queue([a],[])
    | Queue(left,right) -> Queue(left,a::right)
  let take queue = 
    match queue with
      Queue([], _) -> raise Empty
    | Queue(a::[], rrest) -> (a, Queue (List.rev rrest, []))
    | Queue(a::lrest, right) -> (a, Queue (lrest, right))
  (*ここまで*)
  let peek = function
      Queue([], _) -> raise Empty
    | Queue(x :: _, _) -> x
end