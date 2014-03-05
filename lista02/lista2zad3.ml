let rec mapX f list = match list with
	  []		-> []
	| h :: rest -> (mapX f rest) @ [(f h)]

let mapT f list = let rec mapTtmp f list acc = match list with
	  []		-> acc
	| h :: rest -> mapT f rest ((f h) :: acc)
in mapTtmp f list []