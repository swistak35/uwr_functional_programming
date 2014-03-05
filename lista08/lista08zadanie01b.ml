module type PQUEUE =
sig
  type priority
  type 'a t
    
  exception EmptyPQueue
  
  val empty  : 'a t
  val insert : 'a t -> priority -> 'a -> 'a t
  val remove : 'a t -> priority * 'a * 'a t
end

module type ORDTYPE =
sig
  type t
  val compare : t -> t -> int
end

module OrdInt : ORDTYPE with type t = int = struct
	type t = int
	let compare x y =
		if x = y
		then 0
		else (if x > y
				then 1
				else -1)
end

(*
	Dlaczego 
		module OPqueue : PQUEUE = functor (Ordt: ORDTYPE) -> struct
	daje mismatch jestem w stanie zrozumieć
	ale dlaczego 
		module OPqueue = functor (Ordt: ORDTYPE) : PQUEUE -> struct
	jest niepoprawne składniowo? Jak uzyskać ten sam efekt, ale ze słowem "functor"?
*)

module OPqueue (Ordt: ORDTYPE) : PQUEUE with type priority = Ordt.t = struct
	type priority = Ordt.t
	type 'a t = (priority * 'a) list

	exception EmptyPQueue

	let empty = ([] : 'a t)
	let insert queue pr x =
		(pr,x)::queue
	let find_max_p lst = match lst with
		| []		-> raise EmptyPQueue
		| hd::tl 	->
			List.fold_right
			(fun (p,_) maxp -> (if Ordt.compare p maxp = 1 then p else maxp))
			tl (fst hd)
	let rec remove queue = 
		let rec _remove qlist maxp = match qlist with
			| []			-> raise EmptyPQueue
			| (p,x)::qrest	->
				if Ordt.compare maxp p = 0
				then (p,x,qrest)
				else (let (resp, resx, rest) = _remove qrest maxp
								in (resp, resx, (p,x)::rest))
		in _remove queue (find_max_p queue)
end

module Qint = OPqueue(OrdInt);;

let p0 = Qint.empty
let p1 = Qint.insert p0 3 'f'
let p2 = Qint.insert p1 5 'z'
let p3 = Qint.insert p2 1 'a'
let check1 = let (p, x, _) = Qint.remove p3 in p = 5 && x = 'z'

let rec builder queue =
	if queue = Qint.empty
	then []
	else let (_, x, rest) = Qint.remove queue in x :: (builder rest)
let sort list = 
	let queue = List.fold_right (fun x acc -> Qint.insert acc x x) list Qint.empty
	in List.rev (builder queue)

let check2 = sort [7;2;8;4;3;6] = [2;3;4;6;7;8]
