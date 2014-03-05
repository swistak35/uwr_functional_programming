let rec tails list = match list with
	  []	-> [[]]
	| h::t 	-> let prev_tails = tails t in (h::(List.hd prev_tails))::prev_tails

let rec tails2 list = match list with
	  []	-> []
	| [x]	-> [[x]]
	| h::t 	-> let prev_tails = tails2 t in (h::(List.hd prev_tails))::prev_tails

let rec inits list = match list with
	  []	->	[[]]
	| h::t 	-> 	[] :: List.map (fun l -> h::l) (inits t)

let rec inits2 list = match list with
	  []	->	[]
	| [x]	->	[[x]]
	| h::t 	-> 	[h] :: List.map (fun l -> h::l) (inits2 t)

let tails_1 = tails [] = [[]]
let tails_2 = tails [1] = [[1];[]]
let tails_3 = tails [1;2] = [[1;2];[2];[]]
let tails_4 = tails [1;2;3] = [[1;2;3];[2;3];[3];[]]

let tails2_1 = tails2 [] = []
let tails2_2 = tails2 [1] = [[1]]
let tails2_3 = tails2 [1;2] = [[1;2];[2]]
let tails2_4 = tails2 [1;2;3] = [[1;2;3];[2;3];[3]]

let inits_1 = inits [] = [[]]
let inits_2 = inits [1] = [[];[1]]
let inits_3 = inits [1;2] = [[];[1];[1;2]]
let inits_4 = inits [1;2;3] = [[];[1];[1;2];[1;2;3]]

let inits2_1 = inits2 [] = []
let inits2_2 = inits2 [1] = [[1]]
let inits2_3 = inits2 [1;2] = [[1];[1;2]]
let inits2_4 = inits2 [1;2;3] = [[1];[1;2];[1;2;3]]

let myTests = [
	("tails_1", tails_1);
	("tails_2", tails_2);
	("tails_3", tails_3);
	("tails_4", tails_4);
	("tails2_1", tails2_1);
	("tails2_2", tails2_2);
	("tails2_3", tails2_3);
	("tails2_4", tails2_4);
	("inits_1", inits_1);
	("inits_2", inits_2);
	("inits_3", inits_3);
	("inits_4", inits_4);
	("inits2_1", inits2_1);
	("inits2_2", inits2_2);
	("inits2_3", inits2_3);
	("inits2_4", inits2_4);
]
