(* Wersja korzystajaca z wersji list leniwych z wykladu *)
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist)

(* Funkcje z wykładu *)
let rec lfrom k = LCons (k, function () -> lfrom (k+1));;
let rec ltakeWithTail = function
	  (0, xq) -> ([],xq)
	| (_, LNil) -> ([],LNil)
	| (n, LCons(x,xf)) ->
		let (l,tail) = ltakeWithTail(n-1, xf())
		in (x::l,tail)
let rec ltake = function
	  (0, _) -> []
	| (_, LNil) -> []
	| (n, LCons(x,xf)) -> x::ltake(n-1, xf())

(* Zbiegające do pi *)
let lazy_pi =
	let rec _lazy_pi n k z =
		let res = (n +. z *. (1. /. k)) in
		LCons (4. *. res, function () -> _lazy_pi res (k +. 2.) (z *. (-1.)))
	in _lazy_pi 0. 1. 1.

(* Z tresci zadania, tzn. z fragmentu "dowolny strumień x1,x2,x3,..." rozumiem,
	że 3 początkowe elementy mamy gwarantowane, że są zdefiniowane
*)
let lazy_f3 f llist = 
	let rec _lazy_f3 f n1 n2 llist = match llist with
		  LNil 			-> LNil
		| LCons(n3,xf) 	-> LCons (f n1 n2 n3, function () -> _lazy_f3 f n2 n3 (xf()))
	in let ([x1;x2],xt) = ltakeWithTail (2,llist)
	in _lazy_f3 f x1 x2 xt

let add3 x1 x2 x3 = x1 + x2 + x3
let euler x y z = z -. (y -. z)**(2.) /. (x -. 2. *. y +. z)

(* Szybciej zbiega niz lazy_pi *)
let lazy_pie = lazy_f3 euler lazy_pi