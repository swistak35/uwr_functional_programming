module MyArray = struct
	type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree 
	type 'a array = Array of 'a btree * int

	(* aempty : 'a array, tablica pusta; *)
	let aempty = Array(Leaf, 0)

	(* searching and building the tree *)
	let rec search tree k f = match tree with
		| Leaf				-> f tree
		| Node(lt,x,rt) 	->
			if k = 1 then f tree
			else if (k mod 2 = 0) then Node(search lt (k / 2) f, x, rt)
			else if (k mod 2 = 1) then Node(lt, x, search rt (k / 2) f)
			else failwith "Internal error"

	(* asub : 'a array -> int -> 'a, pobranie składowej o zadanym indeksie; *)
	let rec _asub tree k = match tree with
		| Leaf				-> failwith "Array index finding error"
		| Node(lt,x,rt) 	->
			if k = 1 then x
			else if (k mod 2 = 0) then _asub lt (k / 2) 
			else if (k mod 2 = 1) then _asub rt (k / 2)
			else failwith "Internal error"
	let asub arr k = let Array(tree,_) = arr in _asub tree k

	(* aupdate : 'a array -> int -> 'a -> 'a array, modyfikacja składowej o zadanym indeksie; *)
	let aupdate arr k newx =
		let Array(tree, m) = arr
		in let faupdate tree = match tree with
			  Leaf 				-> failwith "aupdate: error"
			| Node(lt,x,rt) 	-> Node(lt,newx,rt)
		in Array(search tree k faupdate, m)

	(* ahiext : 'a array -> 'a -> 'a array, rozszerzenie tablicy o jedną składową; *)
	let ahiext arr newx =
		let Array(tree, m) = arr
		in let fahiext tree = match tree with
			  Leaf 			-> Node(Leaf,newx,Leaf)
			| Node(_,_,_) 	-> failwith "ahiext: error"
		in Array(search tree (m+1) fahiext, m+1)

	(* ahirem : 'a array -> 'a array, usunięcie składowej o najwyższym indeksie. *)
	let ahirem arr =
		let Array(tree, m) = arr
		in let fahirem tree = match tree with
			  Leaf 			-> failwith "ahirem: error"
			| Node(_,_,_) 	-> Leaf
		in Array(search tree m fahirem, m-1)
	end;;

let run_tests =
	let test_a0 = MyArray.aempty
	in let test_a1 = MyArray.ahiext test_a0 1
	in let test_a2 = MyArray.ahiext test_a1 2
	in let test_a3 = MyArray.ahiext test_a2 3
	in let test_a4 = MyArray.ahiext test_a3 4
	in let test_a5 = MyArray.ahiext test_a4 5
	in let test_a6 = MyArray.ahiext test_a5 6
	in let test_a7 = MyArray.ahiext test_a6 7
	in let test_a8 = MyArray.ahiext test_a7 8
	in let check1 = test_a8 = MyArray.Array(MyArray.Node(MyArray.Node(MyArray.Node(MyArray.Node(MyArray.Leaf,8,MyArray.Leaf),4,MyArray.Leaf),2,MyArray.Node(MyArray.Leaf,6,MyArray.Leaf)),1,MyArray.Node(MyArray.Node(MyArray.Leaf,5,MyArray.Leaf),3,MyArray.Node(MyArray.Leaf,7,MyArray.Leaf))),8)
	in let check2 = test_a7 = MyArray.ahirem test_a8
	in let check3 = test_a6 = MyArray.ahirem test_a7
	in let check4 = MyArray.aupdate test_a8 8 42 = MyArray.Array(MyArray.Node(MyArray.Node(MyArray.Node(MyArray.Node(MyArray.Leaf,42,MyArray.Leaf),4,MyArray.Leaf),2,MyArray.Node(MyArray.Leaf,6,MyArray.Leaf)),1,MyArray.Node(MyArray.Node(MyArray.Leaf,5,MyArray.Leaf),3,MyArray.Node(MyArray.Leaf,7,MyArray.Leaf))),8)
	in let range = [1;2;3;4;5;6;7;8]
	in let check5 = range = List.map (MyArray.asub test_a8) range
	in let check_all = [check1;check2;check3;check4;check5]
	in check_all