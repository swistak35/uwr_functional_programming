let rec funa n = match n with
		0	-> 1
	| 	1 	-> 2
	| 	n	-> 2 * funa (n-2) - funa (n-1) + 1

let () = Printf.printf "Wynik: %d\n" (funa 40)