#use "../myUnit.ml"

let rec merge cmp l1 l2 = match l1 with
	  [] -> l2
	| (h1 :: t1) -> match l2 with
		  [] -> l1
		| (h2 :: t2) -> if cmp h1 h2 then h1 :: merge cmp t1 l2 else h2 :: merge cmp l1 t2

let merge2 cmp l1 l2 = let rec mergeTmp cmp l1 l2 acc = match l1 with
  	  [] -> List.rev acc @ l2
	| (h1 :: t1) -> match l2 with
		  [] -> List.rev acc @ l1
		| (h2 :: t2) -> if cmp h1 h2 then mergeTmp cmp t1 l2 (h1 :: acc) else mergeTmp cmp l1 t2 (h2 :: acc)
in mergeTmp cmp l1 l2 []

let rec halve list = match list with
	  []			-> ([],[])
	| [x]			-> ([],[x])
	| [x;y]			-> ([x],[y])
	| h1::h2::rest	-> let (part1, part2) = halve rest in (h1::part1,h2::part2)

let rec mergesort cmp list = match list with
	  []	->	[]
	| [x] 	->	[x]
	| listp ->  let (part1, part2) = halve listp in merge2 cmp (mergesort cmp part1) (mergesort cmp part2)

(* Testy *)
let halve_1 = halve [] = ([],[])
let halve_2 = (halve [1] = ([1],[])) || (halve [1] = ([],[1]))
let halve_3 = (halve [1;2] = ([1],[2])) || (halve [1;2] = ([2],[1]))
let halve_4 = let (part1, part2) = halve [1;2;3;4;5] in
	((List.length part1 = 3) && (List.length part2 = 2)) ||
	((List.length part1 = 2) && (List.length part2 = 3))
let halve_5 = let (part1, part2) = halve [1;2;3;4;5;6] in ((List.length part1 = 3) && (List.length part2 = 3))

let merge_1 = merge (<) [1;3] [2;4] = [1;2;3;4]
let merge_2 = merge (>) [3;1] [4;2] = [4;3;2;1]
let merge_3 = merge (<) [2] [1] = [1;2]
let merge_4 = merge (<)	[1] [] = [1]
let merge_5 = merge (<)	[] [1] = [1]

let merge2_1 = merge2 (<) [1;3] [2;4] = [1;2;3;4]
let merge2_2 = merge2 (>) [3;1] [4;2] = [4;3;2;1]
let merge2_3 = merge2 (<) [2] [1] = [1;2]
let merge2_4 = merge2 (<) [1] [] = [1]
let merge2_5 = merge2 (<) [] [1] = [1]

let mergesort_1 = mergesort (<) [] = []
let mergesort_2 = mergesort (<) [1] = [1]
let mergesort_3 = mergesort (<) [2;1] = [1;2]
let mergesort_4 = mergesort (>) [1;2] = [2;1]
let mergesort_5 = mergesort (<) [7;3;4;6;9;1;2;5;8] = [1;2;3;4;5;6;7;8;9]

let myTests = [
	("halve_1", halve_1);
	("halve_2", halve_2);
	("halve_3", halve_3);
	("halve_4", halve_4);
	("halve_5", halve_5);
	("merge_1", merge_1);
	("merge_2", merge_2);
	("merge_3", merge_3);
	("merge_4", merge_4);
	("merge_5", merge_5);
	("merge2_1", merge2_1);
	("merge2_2", merge2_2);
	("merge2_3", merge2_3);
	("merge2_4", merge2_4);
	("merge2_5", merge2_5);
	("mergesort_1", mergesort_1);
	("mergesort_2", mergesort_2);
	("mergesort_3", mergesort_3);
	("mergesort_4", mergesort_4);
	("mergesort_5", mergesort_5);
];;
