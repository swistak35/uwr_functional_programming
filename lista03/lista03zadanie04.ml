module MyMatrix =
	struct
		let check mat = let n = List.length mat in List.for_all (fun row -> List.length row = n) mat
		let fetch_col mat n = if check mat then List.map (fun row -> List.nth row n) mat else failwith "Not a matrix"
		let transp mat = if check mat then List.mapi (fun n _ -> fetch_col mat n) mat else failwith "Not a matrix"
		let zip l1 l2 = List.combine l1 l2
		let zipf f l1 l2 = List.map (fun (x1,x2) -> f x1 x2) (zip l1 l2)
		let zipf2 f l1 l2 = if List.length l1 = List.length l2 then
				List.fold_right2 (fun x y acc -> (f x y)::acc) l1 l2 []
			else
				failwith "Wrong length"
		let zip2 l1 l2 = zipf2 (fun x y -> (x,y)) l1 l2
		let sum lst = List.fold_right (+.) lst 0.0
		let dot_product l1 l2 = sum (zipf ( *. ) l1 l2)
		let mult_vec vec mat = List.map (fun col -> dot_product vec col) (transp mat)
		let mult_mat mat1 mat2 = List.map (fun row -> mult_vec row mat2) mat1
	end;;

(* Tests *)
let test_mat_1 = [[1.;2.;3.];[3.;4.;5.];[5.;6.;7.]]
let test_mat_2 = [[1.;2.;3.];[2.;3.;4.;6.];[3.;4.;5.]]
let test_mat_3 = [[1.;0.;0.];[0.;1.;0.];[0.;0.;1.]]
let test_mat_4 = [[1.;2.;1.];[0.;0.;1.];[1.;0.;2.]]

let check_1 = MyMatrix.check test_mat_1 = true
let check_2 = MyMatrix.check [[1.;2.;3.];[2.;3.;4.]] = false
let check_3 = MyMatrix.check [[1.;2.;3.];[2.;3.;4.;5.];[3.;4.;5.]] = false

let fetch_col_1 = MyMatrix.fetch_col test_mat_1 0 = [1.;3.;5.]
let fetch_col_2 = MyMatrix.fetch_col test_mat_1 1 = [2.;4.;6.]
let fetch_col_3 = MyMatrix.fetch_col test_mat_1 2 = [3.;5.;7.]
let fetch_col_4 = try (MyMatrix.fetch_col test_mat_2 0 = []) with
				  Failure _ -> true
				| x -> false
let fetch_col_5 = try (MyMatrix.fetch_col test_mat_1 5 = []) with
				  Failure _ -> true
				| x -> false

let transp_1 = MyMatrix.transp test_mat_1 = [[1.;3.;5.];[2.;4.;6.];[3.;5.;7.]]


let zip_1 = MyMatrix.zip [1;2;3] ["a";"b";"c"] = [(1,"a");(2,"b");(3,"c")]
let zip_2 = try (MyMatrix.zip [1;2;3;4] [7;8;9] = []) with
				  Failure _ -> true
				| _ -> false
let zip_3 = MyMatrix.zip [] [] = []

let zip2_1 = MyMatrix.zip2 [1;2;3] ["a";"b";"c"] = [(1,"a");(2,"b");(3,"c")]
let zip2_2 = try (MyMatrix.zip2 [1;2;3;4] [7;8;9] = []) with
				  Failure _ -> true
				| _ -> false
let zip2_3 = MyMatrix.zip2 [] [] = []

let zipf_1 = MyMatrix.zipf ( +. ) [1.;2.;3.] [4.;5.;6.] = [5.;7.;9.]
let zipf_2 = MyMatrix.zipf (fun x y -> (x,y)) [1;2;3] ["a";"b";"c"] = [(1,"a");(2,"b");(3,"c")]

let zipf2_1 = MyMatrix.zipf2 ( +. ) [1.;2.;3.] [4.;5.;6.] = [5.;7.;9.]
let zipf2_2 = MyMatrix.zipf2 (fun x y -> (x,y)) [1;2;3] ["a";"b";"c"] = [(1,"a");(2,"b");(3,"c")]

let mult_vec_1 = MyMatrix.mult_vec [1.;2.] [[2.;0.];[4.;5.]] = [10.;10.]
let mult_vec_2 = MyMatrix.mult_vec [3.;2.;1.] test_mat_1 = [14.;20.;26.]

let mult_mat_1 = MyMatrix.mult_mat test_mat_1 test_mat_3 = test_mat_1
let mult_mat_2 = MyMatrix.mult_mat test_mat_1 test_mat_4 = [[4.;2.;9.];[8.;6.;17.];[12.;10.;25.;]]

let myTests = [
	("check_1", check_1);
	("check_2", check_2);
	("check_3", check_3);
	("fetch_col_1", fetch_col_1);
	("fetch_col_2", fetch_col_2);
	("fetch_col_3", fetch_col_3);
	("transp_1", transp_1);
	("zip_1", zip_1);
	("zip_2", zip_2);
	("zip_3", zip_3);
	("zip2_1", zip2_1);
	("zip2_2", zip2_2);
	("zip2_3", zip2_3);
	("zipf_1", zipf_1);
	("zipf_2", zipf_2);
	("zipf2_1", zipf2_1);
	("zipf2_2", zipf2_2);
	("mult_vec_1", mult_vec_1);
	("mult_vec_2", mult_vec_2);
	("mult_mat_1", mult_mat_1);
	("mult_mat_2", mult_mat_2);
]
