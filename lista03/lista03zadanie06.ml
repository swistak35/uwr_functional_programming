(*
	Logik P nie wie co to za liczby, więc przynajmniej jedna z nich nie jest pierwsza.
	Gdyby byly obie pierwsze, to x*y dawałyby jednoznaczny wynik
	Logik S wiedzial o tym, wiec jego suma byla taka, ze jak wezmiemy wszystkie
	mozliwe pary (x,y) takie ze x+y = s, to wsrod tych par nie ma pary dwoch liczb pierwszych.
	Stad, mozliwe sumy jaka mogl miec S to Xs = [6,11,17,23,27,29,35,37,41,47,51,53,57,59,65,67,71,77,79,83,87,89,93,95,97]
	Logik P to wie, wiec moze znalezc swoje liczby nast. sposobem:
		wez sume s' nalezaca do Xs.
		wygeneruj wszystkie mozliwe pary (x,y) takie ze x*y = p
		poszukaj wsrod tych par takie liczby, ze x+y = s'
		trzymaj kciuki ze jest tylko jedna taka para, jesli tak to ja wypluj
		skoro jest tylko jedna taka para (x,y)
	Logik S moze znalezc ta liczbe sprawdzajac wszystkie pary (x,y), t.ze x+y = s, 
	rozkladac je na czynniki pierwsze i sprawdzic czy da sie z nich po dodaniu otrzymac
	sume z Xs, ale rozna od s, jesli tak, to odpada.
*)

let rec range start stop = if start > stop then [] else start :: range (start+1) stop
let sum_pairs n = List.map (fun x -> (x,n-x)) (range 2 ((n-1)/2))
let factorization n =
	let rec tmp n i = match n with
		  1 -> []
		| n -> if n mod i = 0 then i::tmp (n/i) i else tmp n (i+1)
	in tmp n 2
let is_prime n = List.length (factorization n) = 1
let good_pair (x,y) = not (y = x*x || (is_prime x && is_prime y))
let good_sum n = List.for_all good_pair (sum_pairs n)
let mySums = List.filter good_sum (range 5 99)
let factors sum = List.map (fun (x,y) -> factorization (x*y)) (List.filter good_pair (sum_pairs sum))
let all_factorizations = List.concat (List.map factors mySums)
let uniq lst = let rec tmp elements acc = match elements with
		  []	 -> acc
		| hd::tl -> if List.mem hd acc then tmp tl acc else tmp tl (hd::acc)
	in tmp lst []
let count element lst = List.length (List.filter (fun x -> x = element) lst)
let nice_factorizations = List.filter (fun x -> count x all_factorizations = 1) (uniq all_factorizations)

(* napisac kod ktory je generuje *)
(* let mySums = [6,11,17,23,27,29,35,37,41,47,51,53,57,59,65,67,71,77,79,83,87,89,93,95,97] *)