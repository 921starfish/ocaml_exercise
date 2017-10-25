module type TABLE2 =
sig
  type ('a, 'b) t
  val empty : ('a, 'b) t
  val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val retrieve : 'a -> ('a, 'b) t -> 'b option
  val dump : ('a, 'b) t -> ('a * 'b) list
end

module Table : TABLE2 = 
struct
  type ('a, 'b) t = Lf | Br of ('a * 'b) * ('a, 'b) t * ('a, 'b) t

  let empty = Lf

  let rec add key datum table =
    match table with
      Lf -> Br ((key, datum), Lf, Lf)
    | Br ((key', datum'), left, right) -> 
      if key = key' then 
        Br((key', datum'), left, right)
      else if key < key' then 
        Br((key', datum'), add key datum left, right)
      else 
        Br((key', datum'), left, add key datum right)

  let rec retrieve key table =
    match table with
      Lf -> None
    | Br ((key', datum'), left, right) ->
      if key = key' then 
        Some datum' 
      else if key < key' then 
        retrieve key left 
      else 
        retrieve key right

  let rec dump table =
    match table with
      Lf -> []
    | Br((key, contents), left, right) -> (dump left) @ (key, contents) :: (dump right)
end