(* Z wykładu *)
type 'a lnode = {item: 'a; mutable next: 'a lnode}
let mk_circular_list e = let rec x = {item=e; next=x} in x
let insert_tail e l = let x = {item=e; next=l.next} in l.next <- x; x
let first ln = (ln.next).item
let last ln = ln.item
let elim_head l = l.next <- (l.next).next; l

let rec range a b = if a > b then [] else a :: (range (a+1) b)

let crange n =
	let clist = mk_circular_list 1
	in List.fold_right insert_tail (List.rev (range 2 n)) clist

let rec poor_joseph n m =
	let rec _poor clist m' =
		let fst = first clist
		in if clist.next == clist
		then [fst]
		else if m = m'
		then fst :: (_poor (elim_head clist) 1)
		else _poor clist.next (m' + 1)
	in _poor (crange n) 1

let check1 = poor_joseph 7 3 = [3; 6; 2; 7; 5; 1; 4]
let check2 = poor_joseph 9 2 = [2; 4; 6; 8; 1; 5; 9; 7; 3]