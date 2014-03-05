type 'a mtree = MNode of 'a * 'a forest
and 'a forest = EmptyForest | Forest of 'a mtree * 'a forest

let rec appendForests frst1 frst2 = match frst1 with
	  EmptyForest	-> frst2
	| Forest(t,rst) -> Forest(t,appendForests rst frst2)
let concatForests forests = List.fold_right appendForests forests EmptyForest

let rec findSome f frst = match frst with
	  EmptyForest	-> None
	| Forest(h,tl)	-> match (f h) with
		  None 		-> findSome f tl
		| Some x 	-> Some x

let rec _search f lst acc = match lst with
	  EmptyForest					-> (None, acc)
	| Forest(MNode(n, forest),tl)	->
		if f n
		then (Some n, [])
		else _search f tl (forest::acc)

let bfs f (MNode(n,forest)) =
	let rec _bfs f forest = match (_search f forest []) with
		  (None, nfrst)		-> (
	  		match concatForests nfrst with
	  			  EmptyForest	-> None
	  			| nfrst 		-> _bfs f nfrst
	  		)
		| (Some n, _)		-> Some n
	in if f n then Some n else _bfs f forest

let rec dfs f (MNode(n,forest)) = if f n then Some n else findSome (dfs f) forest

let test_tree1 = (MNode(1,Forest(MNode(2,Forest(MNode(15,EmptyForest),EmptyForest)),Forest(MNode(13,EmptyForest),EmptyForest))))
let test_tree2 = (MNode(11,Forest(MNode(2,Forest(MNode(15,EmptyForest),EmptyForest)),Forest(MNode(13,EmptyForest),EmptyForest))))
let test_tree3 = (MNode(1,Forest(MNode(2,Forest(MNode(5,EmptyForest),EmptyForest)),Forest(MNode(3,EmptyForest),EmptyForest))))
let test_bfs1 = bfs ((<) 10) test_tree1 = Some 13
let test_bfs2 = bfs ((<) 10) test_tree2 = Some 11
let test_bfs2 = bfs ((<) 10) test_tree3 = None
let test_dfs1 = dfs ((<) 10) test_tree1 = Some 15
let test_dfs2 = dfs ((<) 10) test_tree2 = Some 11
let test_dfs2 = dfs ((<) 10) test_tree3 = None