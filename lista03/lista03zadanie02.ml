let horner x list = List.fold_right (fun e acc -> acc * x + e) list 0

let rec horner2 x list = match list with
	  [y]	-> y
	| h::t 	-> h + x * (horner2 x t)
