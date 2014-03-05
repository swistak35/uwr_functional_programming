type 'a htree = Leaf of 'a * int | Node of 'a htree * int * 'a htree

let take_int = function Leaf(_,x) -> x | Node(_,x,_) -> x

let cmp h1 h2 = take_int h1 - take_int h2

let build_leaflist lst = List.map (fun (c,v) -> Leaf(c,v)) lst

let build_tree lst = let rec _build_tree lst = match lst with
		  [x]		-> x
		| h1::h2::t ->
			let new_node = (Node(h1, take_int h1 + take_int h2, h2))
			in _build_tree (List.sort cmp (new_node::t))
	in _build_tree (List.sort cmp (build_leaflist lst))

let rec build_assoc tree = match tree with
	  Leaf(c,_) 	-> [(c,[])]
	| Node(lt,_,rt) ->
		let lt_res = List.map (fun (c,xs) -> (c,0::xs)) (build_assoc lt)
		and rt_res = List.map (fun (c,xs) -> (c,1::xs)) (build_assoc rt)
		in lt_res @ rt_res

let code_text lst tree =
	let rec _code_text lst assoc = match lst with
		  [] 	-> []
		| h::t 	-> (List.assoc h assoc)::(_code_text t assoc)
	in List.concat (_code_text lst (build_assoc tree))

let decode_text lst tree = 
	let rec decode_step lst step = match step with
		  Leaf(c,_) 	-> c::(decode_step lst tree)
		| Node(lt,_,rt) -> process_bit lst lt rt
	and process_bit lst lt rt = match lst with
		  [] 	-> []
		| h::t 	-> decode_step t (if h = 0 then lt else rt)
	in decode_step lst tree

let text = ['a'; 'b'; 'c'; 'a'; 'a']

let test_list1 = [('a', 27); ('b', 65); ('c', 3); ('d', 3); ('e', 2)]
let test_tree1 = build_tree test_list1

let result1 = text = decode_text (code_text text test_tree1) test_tree1

let test_list2 = [('a', 90); ('b', 3); ('c', 3); ('d', 2); ('e', 2)]
let test_tree2 = build_tree test_list2
let result2 = text = decode_text (code_text text test_tree2) test_tree2