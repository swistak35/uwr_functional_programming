type 'a btree = Leaf of 'a | Node of 'a btree * 'a * 'a btree

let rec _dfs_count tree n = match tree with
	  Leaf(_)		-> (Leaf n, n+1)
	| Node(lt,_,rt) ->
	  		let (lt2, n2) = _dfs_count lt (n+1)
	  		in let (rt2, n3) = _dfs_count rt n2
	  		in (Node(lt2,n,rt2), n3)
let dfs_count tree = fst (_dfs_count tree 1)

let sons tree = match tree with Some (Node(lt,_,rt)) -> [Some lt; Some rt] | _ -> [None; None]

let rec number n forest = match forest with
	  []			-> (n, [])
	| None::tl 		-> let (n', tl') = number n tl in (n', None::tl')
	| Some(_)::tl 	-> let (n', tl') = number (n+1) tl in (n', (Some (n+1))::tl')

let rec rebuild forest numlist numforest = match forest with
	  [] 						-> []
	| None::tl 					-> 
		let _::tlNumlist = numlist
		and _::_::tlNumforest = numforest
		in None::(rebuild tl tlNumlist tlNumforest)
	| Some (Leaf(_))::tl		->
		let (Some n)::tlNumlist = numlist
		and _::_::tlNumforest = numforest
		in (Some (Leaf(n)))::(rebuild tl tlNumlist tlNumforest)
	| Some (Node(_,_,_))::tl 	->
		let (Some n)::tlNumlist = numlist
		in let (Some lt)::(Some rt)::tlNumforest = numforest
		in (Some (Node(lt,n,rt)))::(rebuild tl tlNumlist tlNumforest)

let rec _bfs_count n forest =
	let forest' = List.concat (List.map sons forest)
	in let (n', numlist') = number n forest'
	in 	if (List.for_all ((=) None) forest')
		then (List.map (fun _ -> None) numlist')
		else (let numforest = _bfs_count n' forest'
			  in rebuild forest' numlist' numforest)

let bfs_count tree =
	let [Some root] = rebuild [Some tree] [Some 1] (_bfs_count 1 [Some tree])
	in root
	
let exm_tree1 = Node (Node (Leaf 'a', 'b', Leaf 'c'), 'd', Leaf 'e')
let res_tree1 = Node (Node (Leaf 4, 2, Leaf 5), 1, Leaf 3)
let check1 = bfs_count exm_tree1 = res_tree1

let exm_tree2 = Node(Node(Leaf 'q','x',Node(Node(Leaf 'q','x',Leaf 'q'),'x',Leaf 'q')),'x',Node(Leaf 'q','x',Node(Leaf 'q','x',Leaf 'q')))
let res_tree2 = Node(Node(Leaf  4 , 2 ,Node(Node(Leaf  12, 8 ,Leaf  13), 5 ,Leaf  9 )), 1 ,Node(Leaf  6 , 3 ,Node(Leaf  10, 7 ,Leaf  11)))
let check2 = bfs_count exm_tree2 = res_tree2

let exm_tree3 = Leaf 'x'
let res_tree3 = Leaf 1
let check3 = bfs_count exm_tree3 = res_tree3