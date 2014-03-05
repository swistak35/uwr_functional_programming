type 	wa =
	| Constant of int
	| Var of identifier
	| Add of wa * wa
	| Mul of wa * wa
	| Sub of wa * wa
	| Neg of wa
and 	wb = 
	| TrueValue
	| FalseValue
	| Equal of wa * wa
	| Leq of wa * wa
	| Not of wb
	| And of wb * wb
and 	var_declaration =
	| DecVar of identifier * wa * var_declaration
	| DecVarNone
and 	proc_declaration = 
	| DecProc of identifier * (identifier option) * ws * proc_declaration
	| DecProcNone
and 	ws = 
	| Skip
	| Assgn of identifier * wa
	| Cons of ws * ws
	| Cond of wb * ws * ws
	| While of wb * ws
	| Block of var_declaration * proc_declaration * ws
	| Call of identifier * (wa option)
	| Write of identifier
	| Read of identifier
	| Raise of identifier
	| Try of ws * wcatch
and 	wcatch =
	| RescueNone
	| Rescue of identifier * ws * wcatch
and 	identifier = string
and 	data_type = int