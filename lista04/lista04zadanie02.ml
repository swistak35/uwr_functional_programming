type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree

let diff_gt_1 x y = abs (x-y) > 1
let is_balanced tree = let rec _is_balanced tree = match tree with
		  Leaf					-> (0, true)
		| Node(lt, _, rt)		->
			let (count_lt, bool_lt) = _is_balanced lt
			and (count_rt, bool_rt) = _is_balanced rt
			in if not bool_lt || not bool_rt || diff_gt_1 count_lt count_rt then
				(0, false)
			else
				(count_lt + count_rt + 1, true)
	in snd (_is_balanced tree)

let rec span lst n = match n with
	  0 -> ([], lst)
	| n -> match lst with
		  []		-> ([],[])
		| hd::tl 	-> let (lst1, lst2) = span tl (n-1) in (hd::lst1, lst2)

let halve lst = span lst (List.length lst / 2)

let rec build_tree lst = match lst with
	  [] 	-> Leaf
	| [x] 	-> Node (Leaf, x, Leaf)
	| h::tl	->
		let (part1, part2) = halve tl in
		let lt = build_tree part1 in
		let rt = build_tree part2 in
		Node (lt, h, rt)


