(* Artykuł ładnie wprowadzający w CPS *)
(* http://codebetter.com/matthewpodwysocki/2008/08/13/recursing-on-recursion-continuation-passing/ *)

type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree
let id x = x

let rec prod_cps btree cont = match btree with
	  Leaf 				-> cont 1
	| Node(lt,n,rt) 	-> prod_cps lt (fun lt_prod -> prod_cps rt (fun rt_prod -> cont (n * lt_prod * rt_prod)))
	(* | Node(lt,n,rt) 	-> prod_cps lt (fun lt_prod -> n * lt_prod * (prod_cps rt cont)) *)

let prod btree = prod_cps btree id

let rec prod_cps2 btree cont = match btree with
	  Leaf 				-> cont 1
	| Node(lt,n,rt) 	-> Printf.printf "Zajmuje sie %d\n" n;
		if n = 0
		then 0
		else prod_cps2 lt (fun lt_prod -> prod_cps rt (fun rt_prod -> cont (n * lt_prod * rt_prod)))
		(* else prod_cps2 lt (fun lt_prod -> n * lt_prod * (prod_cps2 rt cont)) *)
let prod2 btree = prod_cps2 btree id


(* Niezbyt zbalansowane drzewko *)
let tree_1 = Node(Node(Leaf,2,Node(Leaf,3,Node(Leaf,5,Leaf))),7,Node(Leaf,11,Leaf))
let tree_1_res = prod tree_1 = 2310
let tree_1_res2 = prod2 tree_1 = 2310

(* Zbalansowane drzewko *)
let tree_2 = Node(Node(Node(Leaf,7,Leaf),3,Node(Leaf,11,Leaf)),2,Node(Node(Leaf,13,Leaf),5,Node(Leaf,17,Leaf)))
let tree_2_res = prod tree_2 = 510510
let tree_2_res2 = prod2 tree_2 = 510510

(* Identyczne jak tree_2, ale zamiast 13 jest 0 *)
let tree_3 = Node(Node(Node(Leaf,7,Leaf),3,Node(Leaf,11,Leaf)),2,Node(Node(Leaf,0,Leaf),5,Node(Leaf,17,Leaf)))
let tree_3_res = prod tree_3 = 0
let tree_3_res2 = prod2 tree_3 = 0
