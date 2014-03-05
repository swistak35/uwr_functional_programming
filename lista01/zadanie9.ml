module MyStream =
	struct
		let hd s = s 0
		let tl s = fun x -> s (x+1)
		let add s n = fun x -> n + s x
		let map f s = fun x -> f (s x)
		let map2 f s1 s2 = fun x -> f (s1 x) (s2 x)
		let replace n a s = fun x -> if x mod n = n-1 then a else s x;;
		let take n s = fun x -> s (n*(x+1) - 1)
		let rec fold f a0 s = fun x -> match x with
			  0 -> f (s 0) a0
			| n -> f (s n) ((fold f a0 s) (x-1))
		let rec tabulate ?(start=0) finish s = match start with
			  st when st > finish  	-> failwith "Start zbyt duzy"
			| st when st = finish 	-> [ s start ]
			| st 					-> (s st) :: tabulate ~start:(st+1) finish s
	end;;