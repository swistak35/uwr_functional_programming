module MyUnit =
	struct
		let runtest (name, testfun) = Printf.printf "Test '%s':\t\t%B\n" name testfun;;

		let rec runtests suite = match suite with
			  []   -> ()
			| (test0 :: rest) -> (runtest test0);runtests rest
	end;;