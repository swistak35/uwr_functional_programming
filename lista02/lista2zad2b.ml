let rec funa_tmp cur n x2 x1 = let result = 2*x2 - x1 + 1 in
	if cur = n then result else funa_tmp (cur+1) n x1 result

let funa2 n = match n with
		0 -> 1
	| 	1 -> 2
	| 	n -> funa_tmp 2 n 1 2

let () = Printf.printf "Wynik: %d\n" (funa2 40)