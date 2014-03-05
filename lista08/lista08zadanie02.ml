module type VERTEX =
sig
  type t
  type label

  val equal : t -> t -> bool
  val create : label -> t
  val label : t -> label  
end

module type EDGE =
sig
	type vertex
	type t
	type label

	val equal : t -> t -> bool
	val create : label -> vertex -> vertex -> t
	val label : t -> label
	val vstart : t -> vertex
	val vend : t -> vertex
end

module type GRAPH =
  sig
  (* typ reprezentacji grafu *)
  type t

  module V : VERTEX
  type vertex = V.t

  module E : EDGE with type vertex = vertex
  type edge = E.t

  (* funkcje wyszukiwania *)
  val mem_v : t -> vertex -> bool
  val mem_e : t -> edge -> bool
  val mem_e_v : t -> vertex -> vertex -> bool
  val find_e : t -> vertex -> vertex -> edge
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list

  (* funkcje modyfikacji *) 
  val empty : t
  val add_e : t -> edge -> t
  val add_v : t -> vertex -> t
  val rem_e : t -> edge -> t
  val rem_v : t -> vertex -> t 

  (* iteratory *)
  val fold_v : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
end

let zlozenie f g x = f (g x)
let (<.>) = zlozenie

module MyGraph : GRAPH with type V.label = int and type E.label = string = struct
	module V : VERTEX with type label = int = struct
		type label = int
		type t = int * label

		let equal v1 v2 = fst v1 = fst v2
		let label v = snd v
		let create = 
			let nid = ref 0 in
			let _create lbl = 
				nid := !nid + 1;
				(!nid, lbl)
			in _create
	end
	type vertex = V.t

	module E : EDGE with type vertex = V.t and type label = string = struct
		type vertex = V.t
		type label = string
		type t = int * label * vertex * vertex

		let equal e1 e2 =
			let (id1,_,_,_) = e1
			and (id2,_,_,_) = e2
			in id1 = id2
		let create =
			let nid = ref 0 in
			let _create lbl v1 v2 = 
				nid := !nid + 1;
				(!nid, lbl, v1, v2)
			in _create
		let label e = let (_,lbl,_,_) = e in lbl
		
		let vstart e = let (_,_,v1,_) = e in v1
		let vend e = let (_,_,_,v2) = e in v2
	end
	type edge = E.t

	type t = vertex list * edge list

	let _edge_fromTo vs ve e = (V.equal vs (E.vstart e)) && (V.equal ve (E.vend e))

	(* val mem_v : t -> vertex -> bool *)
	let mem_v g v = List.exists (V.equal v) (fst g)

	(* val mem_e : t -> edge -> bool *)
	let mem_e g e = List.exists (E.equal e) (snd g)

	(* val mem_e_v : t -> vertex -> vertex -> bool *)
	let mem_e_v g vs ve = List.exists (_edge_fromTo vs ve) (snd g)

	(* val find_e : t -> vertex -> vertex -> edge *)
	let find_e g vs ve = List.find (_edge_fromTo vs ve) (snd g)

	(* val succ_e : t -> vertex -> edge list *)
	let succ_e g v = List.filter (V.equal v <.> E.vstart) (snd g)

	(* val pred_e : t -> vertex -> edge list *)
	let pred_e g v = List.filter (V.equal v <.> E.vend) (snd g)

	(* val succ : t -> vertex -> vertex list *)
	let succ g v = List.map E.vend (succ_e g v)

	(* val pred : t -> vertex -> vertex list *)
	let pred g v = List.map E.vstart (pred_e g v)

	(* val empty : t *)
	let empty = ([],[])

	(* val add_e : t -> edge -> t *)
	let add_e g e = if mem_e g e then g else let (vs, es) = g in (vs, e :: es)
	
	(* val add_v : t -> vertex -> t *)
	let add_v g v = if mem_v g v then g else let (vs, es) = g in (v :: vs, es)
	
	(* val rem_e : t -> edge -> t *)
	let rem_e g e = (fst g, List.filter (not <.> (E.equal e)) (snd g))

	(* val rem_v : t -> vertex -> t  *)
	let rem_v g v =
		let vs = List.filter (not <.> (V.equal v)) (fst g)
		and es = List.filter (fun e -> not ((V.equal (E.vstart e) v) || (V.equal (E.vend e) v))) (snd g)
		in (vs, es)

	(* val fold_v : (vertex -> 'a -> 'a) -> t -> 'a -> 'a *)
	let rec fold_v f g x0 = List.fold_right f (fst g) x0

	(* val fold_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a *)
	let rec fold_e f g x0 = List.fold_right f (snd g) x0
end

let vs = List.map (MyGraph.V.create) [1;2;3;4;5;6;7;8;9]
let g0 = List.fold_left MyGraph.add_v MyGraph.empty vs
let es = List.map (fun (x1,x2) -> MyGraph.E.create "e" (List.nth vs (x1-1)) (List.nth vs (x2-1))) [(2,1);(1,3);(1,4);(1,5);(3,2);(4,3);(4,6);(4,7);(6,8)]
let g1 = List.fold_left MyGraph.add_e g0 es


let rec _dfs pred g v visited =
	if List.mem v visited then (false, visited) else
	if pred v then (true, v::visited) else
	List.fold_left (fun (res, vis) v ->
		let (res', vis') = _dfs pred g v vis in (res || res', vis')
	) (false, v::visited) (MyGraph.succ g v)
let dfs pred g =
	let (res,_) = MyGraph.fold_v (fun v (res, vis) ->
			let (res', vis') = _dfs pred g v vis in (res || res', vis')
		) g (false, [])
	in res

let rec _bfs pred g visited q = match q with
	| []		-> (false, visited)
	| qh::qt	->
		if List.mem qh visited then _bfs pred g visited qt else
		if pred qh then (true, visited) else
		_bfs pred g (qh::visited) (qt @ (MyGraph.succ g qh))
let bfs pred g =
	let (res,_) = MyGraph.fold_v (fun v (res, vis) ->
			let (res', vis') = _bfs pred g vis [v] in (res || res', vis')
		) g (false, [])
	in res

let tmp x = fun v -> MyGraph.V.label v = x
let check1d = List.for_all (fun v -> dfs ((=) v) g1) vs
let check1b = List.for_all (fun v -> bfs ((=) v) g1) vs
let check2d = dfs (tmp 10) g1 = false
let check2b = bfs (tmp 10) g1 = false
let check3d = let (res,_) = _dfs (tmp 4) g1 (List.nth vs 4) [] in res = false
let check3b = let (res,_) = _bfs (tmp 4) g1 [] [List.nth vs 4] in res = false
