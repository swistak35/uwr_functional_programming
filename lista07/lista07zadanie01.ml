(* let rec fix f = f (fix f) *)
(* let rec fix f x = f (fix f) x *)

let fix_silnia = (fun f -> fun n -> if n = 0 then 1 else n * (f (n-1)))

(* let silnia1 = fix fix_silnia *)

let fact =
	let aux = ref (fun n -> n)
	in aux := (fun n -> if n = 0 then 1 else n * (!aux (n-1))); !aux

let fix =
	let aux = ref (fun _ -> failwith "123")
	in (aux := fun f x -> f (!aux f) x); !aux

(* let fact n = *)
	(* let fact2 = ref (fun n -> 1) *)
	(* in fact2 := (fun f n -> f !fact n); !fact2 *)