let horner x list = List.fold_left (fun acc e -> acc * x + e) (List.hd list) (List.tl list)

let horner2 x list = let rec tmp x list acc = match list with
	  []	-> acc
	| h::t	-> tmp x t (acc * x + h)
in tmp x (List.tl list) (List.hd list) 

