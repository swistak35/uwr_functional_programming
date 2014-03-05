type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree 

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist)

let test_tree1a = Node (Node (Leaf 1, Leaf 2), Leaf 3)
let test_tree1b = Node (Leaf 1, Node (Leaf 2, Leaf 3))

let test_tree2a = Node (Node (Leaf 1, Leaf 2), Leaf 3)
let test_tree2b = Node (Leaf 1, Node (Leaf 3, Leaf 2))

let from_some x = match x with Some (x,y) -> y
let rec tree_to_list tree = match tree with
	  Leaf x 		-> [x]
	| Node(lt,rt) 	-> (tree_to_list lt) @ (tree_to_list rt)

let samefringeSimple tree1 tree2 = tree_to_list tree1 = tree_to_list tree2

let rec find_next cont = match (cont()) with
	  LNil 			-> None
	| LCons(tr,xf) 	-> match tr with
		  Leaf x 		-> Some (x, xf)
		| Node(lt,rt)	-> find_next (fun () -> LCons(lt, fun () -> LCons(rt, xf)))

let rec _samefringe cont1 cont2 =
	let res1 = find_next cont1
	and res2 = find_next cont2
	in match (res1, res2) with
		  (None, None) 					-> true
		| (Some (x1,c1), Some (x2,c2)) 	-> 
			if x1 = x2
			then _samefringe c1 c2
			else false

let samefringe tree1 tree2 =
	_samefringe (fun () -> LCons(tree1, fun () -> LNil)) (fun () -> LCons(tree2, fun () -> LNil))