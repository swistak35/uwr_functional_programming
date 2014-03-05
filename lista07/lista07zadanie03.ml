let rec fix f x = f (fix f) x

module Memo = struct
	type ('a, 'b) memo_array = Mnil | Mcons of 'a * 'b * ('a, 'b) memo_array ref
	let empty = ref (Mnil)
	let rec search memo x = match !memo with
		| Mnil 				-> None
		| Mcons(x',y',tl)	-> if x = x' then Some y' else search tl x
	let rec add memo x y =
		memo := Mcons(x,y,ref (!memo))
end;;

let memo_rec f = 
	let memo = Memo.empty in
	let rec memo_f x = match (Memo.search memo x) with
		| None 		-> let res = f memo_f x in (Memo.add memo x res; res)
		| Some res 	-> res
	in memo_f

let rec _fib rec_fib n = match n with
	| 0 -> 1
	| 1 -> 1
	| n -> rec_fib (n - 1) + rec_fib (n - 2)

let fib_memoized = memo_rec _fib
let fib_slow = fix _fib
