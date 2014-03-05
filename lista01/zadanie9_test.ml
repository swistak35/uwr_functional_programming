#use "zadanie9.ml"

(* Przykladowe strumienie *)
let stream1 = (+) 1 (* 1, 2, 3, 4, 5, 6... *)
let stream2 x = x * 3 (* 0, 3, 6, 9, 12... *)

(* Testy *)
let hd_1	= MyStream.hd stream1 = 1
let hd_2	= MyStream.hd stream2 = 0
let tl_1	= MyStream.tl stream1 0 = 2
let tl_2	= MyStream.tl (MyStream.tl stream1) 0 = 3
let add_1	= MyStream.add stream1 3 0 = 4
let add_2	= MyStream.add stream1 3 1 = 5
let map_1	= MyStream.map ((+) 2) stream2 0 = 2
let map_2	= MyStream.map ((+) 2) stream2 1 = 5
let map2_1	= MyStream.map2 (+) stream1 stream2 0 = 1
let map2_2	= MyStream.map2 (+) stream1 stream2 1 = 5
let rep_1	= let nStr = MyStream.replace 3 100 stream1 in (
		nStr 0 = 1 &&
		nStr 1 = 2 &&
		nStr 2 = 100 &&
		nStr 3 = 4 &&
		nStr 4 = 5 &&
		nStr 5 = 100)
let take_1	= let nStr = MyStream.take 4 stream1 in (
		nStr 0 = 4 &&
		nStr 1 = 8 &&
		nStr 2 = 12)
let fold_1	= let nStr = MyStream.fold (+) 0 stream1 in (
		nStr 0 = 1 &&
		nStr 1 = 3 &&
		nStr 2 = 6 &&
		nStr 3 = 10)
let tab_1	= MyStream.tabulate 3 stream1 = [1;2;3;4]
let tab_2	= MyStream.tabulate ~start:2 5 stream1 = [3;4;5;6]
let tab_3	= MyStream.tabulate ~start:2 2 stream1 = [3]
let tab_4	= try (MyStream.tabulate ~start:3 2 stream1 = []) with
				  Failure "Start zbyt duzy" -> true
				| x -> false

let myTests = [
	("MyStream.hd 1", hd_1);
	("MyStream.hd 2", hd_2);
	("MyStream.tl 1", tl_1);
	("MyStream.tl 2", tl_2);
	("MyStream.add 1", add_1);
	("MyStream.add 2", add_2);
	("MyStream.map 1", map_1);
	("MyStream.map 2", map_2);
	("MyStream.map2 1", map2_1);
	("MyStream.map2 2", map2_2);
	("MyStream.rep 1", rep_1);
	("MyStream.take 1", take_1);
	("MyStream.fold 1", fold_1);
	("MyStream.tab 1", tab_1);
	("MyStream.tab 2", tab_2);
	("MyStream.tab 3", tab_3);
	("MyStream.tab 4", tab_4);
];;

(* Testing machine *)
let runtest (name, testfun) = Printf.printf "Test '%s':\t\t%B\n" name testfun;;

let rec runtests suite = match suite with
	  []   -> ()
	| (test0 :: rest) -> (runtest test0);runtests rest

(* let () = runtests myTests *)
