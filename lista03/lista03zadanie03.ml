let pochodna list = let rec tmp list n = match list with
	  [] -> []
	| h::t -> (h *. float(n))::(tmp t (n+1))
in tmp (List.tl list) 1

let pochodna2 list = fst (List.fold_right (
	fun e acc -> let (tail,n) = acc in ((e*.float(n))::tail, n-1)
) (List.tl list) ([],List.length list - 1))