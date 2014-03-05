(*
	Lepsza wersja, napisana przez DaBi'ego,
	find_next ma zdecydowanie ładniejszą postać,
	loop i samefringe są przerobione głównie tak, żeby się typowały
*)

type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree 
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist)

(* Ma działać *)
let test_tree1a = Node (Node (Leaf 1, Leaf 2), Leaf 3)
let test_tree1b = Node (Leaf 1, Node (Leaf 2, Leaf 3))

(* Ma nie działać *)
let test_tree2a = Node (Node (Leaf 1, Leaf 2), Leaf 3)
let test_tree2b = Node (Leaf 1, Node (Leaf 3, Leaf 2))

let rec find_next t cont = 
		match t with
			  Leaf x 		-> LCons (x, cont)
			| Node(lt,rt)	-> find_next lt (fun () -> find_next rt cont)

let rec loop res1 res2 =
			match (res1, res2) with
		  	  (LNil, LNil) 						-> true
			| (LCons (x1,c1), LCons (x2,c2)) 	-> 
				if x1 = x2
				then loop (c1 ()) (c2 ())
				else false
			| (_, _)							-> false

let samefringe tree1 tree2 =
	loop	 
		(find_next tree1 (fun () -> LNil)) 
		(find_next tree2 (fun () -> LNil))
