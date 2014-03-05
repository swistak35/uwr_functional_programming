type 'a llist = LNil | LCons of 'a * (unit -> 'a llist)

let lhd = function LNil -> failwith "lhd" | LCons(x, _) -> x
let ltl = function LNil -> failwith "lhd" | LCons(_, xf) -> xf()
let rec ltake = function
	  (0, _) 			-> []
	| (_, LNil) 		-> []
	| (n, LCons(x,xf)) 	-> x::ltake(n-1, xf())
let rec (@@) ll1 ll2 = match ll1 with
	  LNil 			-> ll2
	| LCons(x,xf) 	-> LCons(x, fun () -> (xf()) @@ ll2)
let rec lmap f = function
	  LNil 			-> LNil
	| LCons(x,xf) 	-> LCons(f x, fun () -> lmap f (xf()))

let rec lfilter pred = function
	  LNil 			->  LNil
	| LCons(x,xf) 	->  if pred x
						then LCons(x, function () -> lfilter pred (xf()))
						else lfilter pred (xf())
let rec toLazyList = function
	  [] 		-> LNil
	| h::tl 	-> LCons(h, fun () -> toLazyList tl)

let depthFirst next x =
	let rec dfs = function
		  [] 		-> LNil
		| (h::t)	-> LCons (h, function () -> dfs (next h @ t))
	in dfs [x]

let breadthFirst next x = 
	let rec bfs = function
		  [] 		-> LNil
		| (h::t)	-> LCons (h, function () -> bfs (t @ (next h)))
	in bfs [x]	

let isQueenSafe oldqs newq = 
	let rec nodiag = function
		  (i, []) 		-> true
		| (i, q::qt)	-> abs(newq-q)<>i && nodiag (i+1,qt)
	in not(List.mem newq oldqs) && nodiag(1,oldqs)

let rec fromTo a b = if a > b then [] else a::(fromTo (a+1) b)

let nextQueen n qs = List.map (function h -> h::qs)
					(List.filter (isQueenSafe qs) (fromTo 1 n))

let isSolution n qs = List.length qs = n

let depthQueen n = lfilter (isSolution n) (depthFirst (nextQueen n) [])
let depthQueen2 n = lfilter (isSolution n) (breadthFirst (nextQueen n) [])

(* let rec someQueen n sol = if List.length sol = n
	then [sol]
	else 	(let good = List.filter (isQueenSafe sol) (fromTo 1 n)
			in List.concat (List.map (function h -> someQueen n (h::sol)) good)) *)

let lappend l1 l2 =
  let rec aux list = match (list()) with
    | LCons (x, xf) -> LCons (x, fun () -> aux xf)
    | _             -> l2()
  in aux l1

let rec lconcat llists = match llists with
	  LNil 			-> LNil
	| LCons(h,tl) 	-> lappend (fun () -> h) (fun () -> lconcat (tl()))

let rec someQueen n sol = if List.length sol = n
	then (Printf.printf "policzone\n"; LCons(sol, fun () -> LNil))
	else 	(let good = List.filter (isQueenSafe sol) (fromTo 1 n)
			in lconcat (lmap (fun h -> someQueen n (h::sol)) (toLazyList good)))