(* Wersja korzystajaca z modulu lazy *)
type 'a lazy_list 	= Nil
					| Cons of 'a * 'a lazy_list lazy_t

(* Funkcje z wyklady, przerobione do korzystania z powyższego typu *)
let rec lfrom k = lazy (Cons (k, lfrom (k+1)))
let rec ltakeWithTail n llist = match (n, Lazy.force llist) with
	  (0, xq) -> ([],xq)
	| (_, Nil) -> ([],Nil)
	| (n, Cons(x,xf)) ->
		let (l, tail) = ltakeWithTail (n-1) xf
		in (x::l,tail)
let rec ltake n llist = match (n, Lazy.force llist) with
	  (0, _) -> []
	| (_, Nil) -> []
	| (n, Cons(x,xf)) -> x::(ltake (n-1) xf)

(* Zbiegające do pi *)
let lazy_pi =
	let rec _lazy_pi n k z =
		let res = (n +. z *. (1. /. k)) in
		lazy (Cons (4. *. res, _lazy_pi res (k +. 2.) (z *. (-1.))))
	in _lazy_pi 0. 1. 1.

(* Z tresci zadania, tzn. z fragmentu "dowolny strumień x1,x2,x3,..." rozumiem,
	że 3 początkowe elementy mamy gwarantowane, że są zdefiniowane
*)
let lazy_f3 f llist = 
	let rec _lazy_f3 f n1 n2 llist = match (Lazy.force llist) with
		  Nil 			-> Nil
		| Cons(n3,xf) 	-> Cons (f n1 n2 n3, lazy (_lazy_f3 f n2 n3 xf))
	in let ([x1;x2],xt) = ltakeWithTail 2 llist
	in _lazy_f3 f x1 x2 (lazy xt)

let add3 x1 x2 x3 = x1 + x2 + x3
let euler x y z = z -. (y -. z)**(2.) /. (x -. 2. *. y +. z)

(* Szybciej zbiega niz lazy_pi *)
let lazy_pie = lazy (lazy_f3 euler lazy_pi)