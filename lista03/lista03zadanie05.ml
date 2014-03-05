(* Zwraca wartosc dla Option, tylko jesli Some *)
let get x = match x with
	  None -> failwith "None option"
	| Some n -> n

(* Zwraca (lst1, lst2) gdzie lst1 to pierwsze n elementow lst, a lst2 to reszta *)
let rec span lst n = match n with
	  0 -> ([], lst)
	| n -> match lst with
		  []		-> ([],[])
		| hd::tl 	-> let (lst1, lst2) = span tl (n-1) in (hd::lst1, lst2)

(* Dzieli liste na 3 czesci wzgledem elementow x_n1 i x_n2 *)
let partition3 lst n1 n2 =
	let (tmp, lst3) = span lst n2 in
	let (lst1, lst2) = span tmp n1 in
	(lst1, List.tl lst2, List.tl lst3)

(* Zwraca parę (k, x_k) dla najmniejszego takiego indeksu k, że f x_(k-1) x_k = true. *)
let find_index2 f lst = let rec tmp f lst i = match lst with
		  []			-> None
		| [x]			-> None
		| h1::h2::tl 	-> if f h1 h2 then Some (i,h2) else tmp f (h2::tl) (i+1)
	in tmp f lst 1

(* Zwraca parę (k, x_k) dla najmniejszego takiego indeksu k, że f x_k = true. *)
let find_index f lst = let rec tmp f lst i = match lst with
		  []		-> None
		| hd::tl	-> if f hd then Some (i,hd) else tmp f tl (i+1)
	in tmp f lst 0

(* Zwraca liste [x; f x; f (f x); f ( f (f x)); ... ], buduje liste dopoki g x = false *)
let rec iter f x g = if g x then [] else x :: iter f (f x) g

(* Zwraca n-elementowa liste [x; f x; f (f x); f ( f (f x)); ... ] *)
let rec itern f x n = if n = 0 then [] else x :: itern f (f x) (n-1)

let next_perm lst = match find_index2 (fun x1 x2 -> x1 > x2) lst with
	  None -> List.rev lst
	| Some (k, x_k) ->
		let (l, x_l) = get (find_index (fun x -> x > x_k) lst) in
		let (lst1, lst2, lst3) = partition3 lst l k in
		(List.rev (lst1 @ (x_k :: lst2))) @ (x_l :: lst3)

let all_perms lst = let lst_next = next_perm lst in
	lst :: iter next_perm lst_next ((=) lst)

let test_1 = let lst = [1;2;3] in lst = List.hd (List.rev (itern next_perm lst 7));;
let test_2 = let lst = [3;4;1;2] in lst = List.hd (List.rev (itern next_perm lst 25));;
let test_3 = all_perms [3;4;2;1] = [[3; 4; 2; 1]; [4; 2; 3; 1]; [2; 4; 3; 1]; [3; 2; 4; 1]; [2; 3; 4; 1]; [4; 3; 1; 2]; [3; 4; 1; 2]; [4; 1; 3; 2]; [1; 4; 3; 2]; [3; 1; 4; 2]; [1; 3; 4; 2]; [4; 2; 1; 3]; [2; 4; 1; 3]; [4; 1; 2; 3]; [1; 4; 2; 3]; [2; 1; 4; 3]; [1; 2; 4; 3]; [3; 2; 1; 4]; [2; 3; 1; 4]; [3; 1; 2; 4]; [1; 3; 2; 4]; [2; 1; 3; 4]; [1; 2; 3; 4]; [4; 3; 2; 1]]