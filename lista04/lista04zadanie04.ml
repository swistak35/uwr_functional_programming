type formID = string
type form 	= Var of formID
			| Not of form
			| And of form * form
			| Or of form * form

let is_some x = match x with None -> false | Some _ -> true
let is_none x = match x with None -> true  | Some _ -> false
let zlozenie f g x = f (g x)
let (<.>) = zlozenie
(* Zwraca unikalne elementy listy *)
let uniq xs = let rec _uniq elements acc = match elements with
		  []	 -> acc
		| hd::tl -> if List.mem hd acc then _uniq tl acc else _uniq tl (hd::acc)
	in _uniq xs []
(* Przeciecie list *)
let intersection (l1, l2) = let rec _intersection l1 l2 = match l1 with
		  [] 	-> []
		| h::tl -> if List.mem h l2 then h::(_intersection tl l2) else (_intersection tl l2)
	in _intersection (uniq l1) (uniq l2)

module Formuly = struct
	(* Zwraca wartosc formuly dla danego wartosciowania (lista asocjacyjna) *)
	let rec eval values form = match form with
		  Var(id) 		-> List.assoc id values
		| Not(f) 		-> not (eval values f)
		| And(f1,f2)	-> (eval values f1) && (eval values f2)
		| Or(f1,f2)		-> (eval values f1) || (eval values f2)
	
	(* Zbiera wszystkie identyfikatory jakie wystepuja w formule *)
	let fetch_vars form = let rec _fetch_vars form = match form with
			  Var(id)		-> [id]
			| Not(f)		-> _fetch_vars f
			| And(f1,f2)	-> (_fetch_vars f1) @ (_fetch_vars f2)
			| Or(f1,f2)		-> (_fetch_vars f1) @ (_fetch_vars f2)
		in uniq (_fetch_vars form)
	
	(* Sprawdzanie czy formula jest tautologia przez sprobowanie wszystkich wartosciowan *)
	let rec check_tautology form = 
		let rec _check_taut vars values form = match vars with
			  [] 		-> if eval values form then None else Some values
			| hd::tl 	-> match (_check_taut tl ((hd,true)::values) form) with
				  Some cntrex 	-> Some cntrex
				| None 			-> match (_check_taut tl ((hd,false)::values) form) with
					  Some cntrex 	-> Some cntrex
					| None 			-> None
		in _check_taut (fetch_vars form) [] form
	
	(* Sprowadzanie do negacyjnej postaci normalnej (nnf?) *)
	let rec neg_normalize form = match form with
		  Var(id) 		-> Var(id)
		| And(f1,f2)	-> And(neg_normalize f1, neg_normalize f2)
		| Or(f1,f2)		-> Or(neg_normalize f1, neg_normalize f2)
		| Not(f) 		-> match f with
			  Var(id)		-> Not(Var(id))
			| And(f1,f2) 	-> Or(neg_normalize (Not(f1)),neg_normalize (Not(f2)))
			| Or(f1,f2) 	-> And(neg_normalize (Not(f1)),neg_normalize (Not(f2)))
			| Not(f)		-> neg_normalize f
	
	(* Sprowadzanie do cnf *)
	let and_normalize form = let rec _and_normalize form = match form with
			  And(f1,f2)	-> And(_and_normalize f1, _and_normalize f2)
			| Or(f1,f2)		-> (
				let nf1 = _and_normalize f1
				and nf2 = _and_normalize f2
				in match (nf1,nf2) with
					  (And(g1,g2),x)			-> And(_and_normalize (Or(g1,x)), _and_normalize (Or(g2,x)))
					| (x,And(g1,g2))			-> And(_and_normalize (Or(g1,x)), _and_normalize (Or(g2,x)))
					| (x,y)						-> Or(x,y)
				)
			| x				-> x
		in _and_normalize (neg_normalize form)
	
	(* Sprowadzanie do dnf *)
	let or_normalize form = let rec _or_normalize form = match form with
			  Or(f1,f2)	-> Or(_or_normalize f1, _or_normalize f2)
			| And(f1,f2)->(
				let nf1 = _or_normalize f1
				and nf2 = _or_normalize f2
				in match (nf1,nf2) with
					  (Or(g1,g2),x)			-> Or(_or_normalize (And(g1,x)), _or_normalize (And(g2,x)))
					| (x,Or(g1,g2))			-> Or(_or_normalize (And(g1,x)), _or_normalize (And(g2,x)))
					| (x,y)					-> And(x,y)
				)
			| x				-> x
		in _or_normalize (neg_normalize form)
	
	(* Przeksztalca drzewo formuly na liste, traktujac jako wezel to co jest w `node`
		(And lub Or) *)
	let rec tree_to_list node form = match form with
		  And (f1,f2) 	-> if node = "And" then (tree_to_list node f1) @ (tree_to_list node f2) else [form]
		| Or (f1,f2) 	-> if node = "Or" then (tree_to_list node f1) @ (tree_to_list node f2) else [form]
		| x				-> [x]

	(* Z listy zmiennych lub ich negacji robi parę list, jedną z identyfikatorami
		tych, ktore byly niezanegowane i tych, ktore byly zanegowane *)
	let rec check_vars vars = match vars with
		  [] 				-> ([],[])
		| (Var id)::tl 		-> let (pos, neg) = check_vars tl in (id::pos, neg)
		| (Not(Var id))::tl -> let (pos, neg) = check_vars tl in (pos, id::neg)

	(* None - tautologia, Some x - niespelnialna dla x *)
	let check_tautology_and form = 
		let cnf = and_normalize form in
		let lst = tree_to_list "And" cnf in
		let lsts = List.map (check_vars <.> (tree_to_list "Or")) lst in
		match (List.filter (fun vars -> intersection vars = []) lsts) with
			  []			-> None
			| (pos,neg)::_ 	->
				let set_vars = ((List.map (fun x -> (x,false)) pos) @ (List.map (fun x -> (x,true)) neg)) in
				Some (List.map (fun x -> if List.mem_assoc x set_vars then (x, List.assoc x set_vars) else (x,true)) (fetch_vars form))

	(* None - sprzeczna, Some x - x spelnia form  *)
	let check_contradict_or form = 
		let dnf = or_normalize form in
		let lst = tree_to_list "Or" dnf in
		let lsts = List.map (check_vars <.> (tree_to_list "And")) lst in
		match (List.filter (fun vars -> intersection vars = []) lsts) with
			  []			-> None
			| (pos,neg)::_ 	-> 
				let set_vars = ((List.map (fun x -> (x,true)) pos) @ (List.map (fun x -> (x,false)) neg)) in
				Some (List.map (fun x -> if List.mem_assoc x set_vars then (x, List.assoc x set_vars) else (x,true)) (fetch_vars form))

end;;


let form_neg_normalize_before = (Not(And(Var "p",Or(Var "q", Not(Not(Not(Not(Var "r"))))))))
let form_neg_normalize_after = Formuly.neg_normalize form_neg_normalize_before
let form_neg_normalize_taut = Formuly.check_tautology form_neg_normalize_before

let form_and_normalize_before = Or(Var "x",And(Var "r",And(Var "q",Var "r")))
let form_and_normalize_after = Formuly.and_normalize form_and_normalize_before
let form_and_normalize_taut = Formuly.check_tautology_and form_and_normalize_before

let form_or_normalize_before = Or(Or(And(Var "p",Not (Var "p")),And(Not (Var "x"),Var "x")),And(Var "y", Not (Var "y")))
let form_or_normalize_after = Formuly.or_normalize form_or_normalize_before
let form_or_normalize_passable = Formuly.check_contradict_or form_or_normalize_before


(* 
Dodatkowe zadanie
Zrobic funkcje ktora w czasie n/2 dla danych
[x1;x2...xn] i [y1;y2...yn]
tworzy liste [(x1,yn), ..., (xn,y1)]
 *)