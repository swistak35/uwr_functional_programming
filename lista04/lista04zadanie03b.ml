type 'a mtree_lst = MTree of 'a * ('a mtree_lst) list

let rec findSome f lst = match lst with
	  []	-> None
	| h::tl	-> match (f h) with
		  None 		-> findSome f tl
		| Some x 	-> Some x

let rec dfs f (MTree(n,forest)) = if f n then Some n else findSome (dfs f) forest

let rec _search f lst acc = match lst with
	  []						-> (None, acc)
	| (MTree(n, forest))::tl	->
		if f n
		then (Some n, [])
		else _search f tl (forest::acc)

let bfs f (MTree(n,forest)) =
	let rec _bfs f forest = match (_search f forest []) with
		  (None, nfrst)		-> (
	  		match List.concat nfrst with
	  			  []	-> None
	  			| nfrst -> _bfs f nfrst
	  		)
		| (Some n, _)		-> Some n
	in if f n then Some n else _bfs f forest



let test_tree1 = (MTree(1,[MTree(2,[MTree(3,[MTree(14,[])]);MTree(5,[])]);MTree(6,[MTree(12,[])])]))
let test_tree2 = (MTree(11,[MTree(2,[MTree(3,[MTree(14,[])]);MTree(5,[])]);MTree(6,[MTree(12,[])])]))
let test_tree3 = (MTree(1,[MTree(2,[MTree(3,[MTree(4,[])]);MTree(5,[])]);MTree(6,[MTree(2,[])])]))
let test_bfs1 = bfs ((<) 10) test_tree1 = Some 12
let test_bfs2 = bfs ((<) 10) test_tree2 = Some 11
let test_bfs2 = bfs ((<) 10) test_tree3 = None
let test_dfs1 = dfs ((<) 10) test_tree1 = Some 14
let test_dfs2 = dfs ((<) 10) test_tree2 = Some 11
let test_dfs2 = dfs ((<) 10) test_tree3 = None

(* bfs (fun x -> Printf.printf "%d\n" x;x > 100) (MTree(1,[MTree(2,[MTree(3,[MTree(14,[])]);MTree(5,[])]);MTree(6,[MTree(12,[])])]));; *)


(* let bfs f ((MTree(n,_)) as rt) = 
	let rec _bfs f (MTree(_,forest)) = match (find2 (fun (MTree(n,_)) -> f n) forest) with
		  None				-> 	findSome (_bfs f) forest
		| Some (MTree(n,_))	->	Some n
	in if f n then Some n else _bfs f rt *)
