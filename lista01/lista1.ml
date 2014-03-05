(* Zadanie 5 *)
let test1 x = 3;;
(* test1 (failwith "test");; *)

(* Zadanie 6 *)
let plus1 x y = x + y;;
let plus2 = fun x -> fun y -> x + y;;
let plus3 = fun x y -> x + y;;

(* Zadanie 7 *)
let int_id (x:int) = x;;
let zlozenie f g x = f (g x);;
let ogolny x = (failwith "error");;

(* Zadanie 8 *)
let id x = x;;
let rec repeat f n = match n with
	  1 -> id
	| _ -> zlozenie f (repeat f (n-1));;

let mnozenie x y = repeat ((+) x) y x;;
let potegowanie x y = repeat (mnozenie x) y x;;
let (^^^) = potegowanie;;