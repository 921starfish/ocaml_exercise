type 'a mlist = MNil | MCons of 'a * 'a mlist ref
type 'a queue = {mutable head : 'a mlist; mutable tail : 'a mlist}
let create () = {head = MNil; tail = MNil}

let add a = function
    {head = MNil; tail = MNil} as q -> let c = MCons (a, ref MNil) in q.head <- c; q.tail <-c
  | {tail = MCons(_, next)} as q -> let c = MCons (a, ref MNil) in next := c; q.tail <- c
  | _ -> failwith "enqueue: input queue broken"

let peek = function
    {head = MNil; tail = MNil} -> failwith "hd: queue is empty"
  | {head = MCons(a, _)} -> a
  | _ -> failwith "hd: queue is broken"
           
(* 以上準備 *)

(* 以下で定義された関数takeはデキューの操作としては不完全 *)

let take = function
    {head = MNil; tail = MNil} -> failwith "dequeue: queue is empty"
  | {head = MCons(a, next)} as q -> q.head <- !next; a
  | _ -> failwith "dequeue: queue is broken"

(* 以下、8.9の解答 *)
let dequeue = function
    {head = MNil; tail = MNil} -> failwith "dequeue: queue is empty"
  | {head = MCons(a, next)} as q -> q.head <- !next; if !next = MNil then q.tail <- MNil; a
  | _ -> failwith "dequeue: queue is broken"
