(* 1 sposób, bardziej ogólny *)
let _fresh n str = str ^ string_of_int(!n)

let counter f = 
	let n = ref 0 in
	let mem_f str = 
		n := !n + 1;
		f n str
	and mem_reset x = 
		n := x;
	in (mem_f, mem_reset)

let (fresh, reset) = counter _fresh

(* 2 sposób, bardziej zwięzły *)
let (fresh2, reset2) = 
	let n = ref 0 in
	let _fresh str =
		n := !n + 1;
		str ^ string_of_int(!n)
	and _reset x =
		n := x
	in (_fresh, _reset)

let check1 =
	fresh "x" = "x1" &&
	fresh "x" = "x2" &&
	fresh "y" = "y3" &&
	reset 10 = () &&
	fresh "z" = "z11"

let check1 =
	fresh2 "x" = "x1" &&
	fresh2 "x" = "x2" &&
	fresh2 "y" = "y3" &&
	reset2 10 = () &&
	fresh2 "z" = "z11"