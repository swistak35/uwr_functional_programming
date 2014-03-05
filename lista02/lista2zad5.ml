let rec insert x list = match list with
	  []	-> [[x]]
	| h::t	-> (x :: list) :: (List.map (fun y -> h :: y) (insert x t) )

let rec perm list = match list with
	  []	-> [[]]
	| h::t	-> List.concat (List.map (insert h) (perm t))

let perm_1 = perm [1] = [[1]]
let perm_2 = perm [1;2] = [[1;2];[2;1]]
let perm_3 = List.length (perm [1;2;3]) = 6
let perm_4 = List.length (perm [1;2;3;4]) = 24

let insert_1 = insert 0 [] = [[0]]
let insert_2 = let res = insert 0 [1] in (res = [[0;1];[1;0]]) || (res = [[1;0];[0;1]])
let insert_3 = List.length (insert 0 [1;2]) = 3
let insert_4 = List.length (insert 0 [1;2;3]) = 4

let myTests = [
	("perm_1", perm_1);
	("perm_2", perm_2);
	("perm_3", perm_3);
	("perm_4", perm_4);
	("insert_1", insert_1);
	("insert_2", insert_2);
	("insert_3", insert_3);
	("insert_4", insert_4);
]