type 'a lazy_list 	= Nil
					| Cons of 'a * 'a lazy_list lazy_t

let isValid stack = true

let rec range n m = if n > m then [] else n :: (range (n+1) m)

(* my mu dajemy [(1,2)] a on nam daje [[(1,2);(3,4)]; ] *)
(* let expand f x =  *)

(* let all_fields = 

let solve_queens n stack acc = 
	let c = n - List.length stack in
	match c with
		0 	-> if isValid stack then Cons(stack, lazy acc) else acc
		c	-> List.fold_left (fun acc x ->
					List.fold_left (fun acc y ->
						solve_queens n ((x,y)::stack) acc
					)
				) acc (range 1 n) *)
