let palindrom xs = let rec _palindrom xs acc count = match count with
	  [] 			-> if acc = xs then true else false
	| [x]			-> if acc = List.tl xs then true else false
	| c1::c2::ct 	-> let hd::tl = xs in _palindrom tl (hd::acc) ct
in _palindrom xs [] xs