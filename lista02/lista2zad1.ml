let rec podlists list = match list with
	[] 			 	-> 	[[]]
	| (h :: rest)	-> 	let sub_podlists = podlists rest in (
						let new_podlists = List.map (fun l -> h :: l) sub_podlists in
						new_podlists @ sub_podlists)

let rec podlists2 list = match list with
	[] 			 	-> 	[[]]
	| (h :: rest)	-> 	let sub_podlists = podlists2 rest in
						List.fold_right (fun l1 acc -> (h :: l1) :: l1 :: acc) sub_podlists []



