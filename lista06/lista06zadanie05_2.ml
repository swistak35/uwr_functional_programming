type regexp = 
			| Atom of char
			| And of regexp * regexp  (*  r1r2     *)
			| Or of regexp * regexp   (*  r1 | r2  *)
			| Star of regexp          (*  r*  *)

(* match_regexp: regexp -> char list -> (char list -> (unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a *)
let rec match_regexp r txt sc fc = match r with
	| Atom(c)		-> (match txt with
			| [] 			-> fc ()
			| h::t 			->
				if h = c
				then sc t fc
				else fc ()
		)
	| And(r1,r2)	-> match_regexp r1 txt (fun str _ -> match_regexp r2 str sc fc) fc
	| Or(r1,r2) 	-> match_regexp r1 txt sc (fun () -> match_regexp r2 txt sc fc)
	| Star(rs)		-> match_regexp rs txt (fun str _ -> match_regexp r str sc fc) (fun () -> sc txt fc)

(* run : regexp -> char list -> bool *)
let run r txt = match_regexp r txt (fun str _ -> str = []) (fun () -> false)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in exp (String.length s - 1) []

let reg1 = And(Atom 'a', And(Atom 'b', Atom 'c'))
let reg2 = And(And(Atom 'a', Star(Or(Atom 'b', Atom 'c'))), Atom 'd')
let txts = List.map explode ["abc"; "qabc"; "aabc"; "abcd"; "bcd"; "abbbcd"; "ad"; "acd"]
let res1 = [true; false; false; false; false; false; false; false]
let res1' = List.map (run reg1) txts
let res2 = [false; false; false; true; false; true; true; true]
let res2' = List.map (run reg2) txts
let check1 = List.for_all2 (=) res1' res1
let check2 = List.for_all2 (=) res2' res2
