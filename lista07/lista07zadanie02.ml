type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref

let rec mappend_copy m1 m2 = match m1 with
	| LMnil 		-> ref m2
	| LMcons(h,t) 	-> ref (LMcons(h, mappend_copy !t m2))

let rec mappend_share m1 m2 = match (!m1) with
	| LMnil 		-> m1 := !m2
	| LMcons(h,t) 	-> mappend_joiner m1 m2 h t
and mappend_joiner m1 m2 h t = match (!t) with
	| LMcons(_,_) 	-> mappend_share t m2
	| LMnil 		-> m1 := LMcons(h,m2)


let rec concat_copy ms = match ms with
	| LMnil 		-> LMnil
	| LMcons(m,t) 	-> mappend_copy m (concat_copy t)


let m1 = LMcons(1, ref (LMcons(2, ref (LMcons(3, ref LMnil)))))
let m2 = LMcons(7, ref (LMcons(8, ref (LMcons(9, ref LMnil)))))
let m1r = ref (LMcons(1, ref (LMcons(2, ref (LMcons(3, ref LMnil))))))
let m2r = ref (LMcons(7, ref (LMcons(8, ref (LMcons(9, ref LMnil))))))
