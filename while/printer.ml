open Wast
open Format

(* 
	Piece of *really* boilerplate code.
*)

let rec print_ws stm = match stm with
	| Skip				-> print_string "Skip"
	| Assgn(lbl, a)		-> begin
			open_box 0;
			print_string "Assgn(";
			print_cut ();
			print_string (lbl ^ ",");
			print_space ();
			print_wa a;
			print_string ")";
			close_box ();
		end
	| Cons(s1,s2)		-> begin
			open_vbox 0;
			print_string "Cons(";
			print_cut ();
				open_box 1;
				print_ws s1;
				print_string ",";
				close_box ();
			print_cut ();
				open_box 1;
				print_ws s2;
				close_box ();
			print_string ")";
			close_box ();
		end
	| Cond(b,s1,s2)		-> begin
			open_box 0;
			print_string "Cond(";
			print_cut ();
			print_wb b;
			print_string ",";
				open_vbox 1;
				print_space ();
				print_ws s1;
				print_string ",";
				print_space ();
				print_ws s2;
				close_box ();
			print_string ")";
		end
	| While(b,s)		-> begin
			open_box 0;
			print_string "While(";
			print_cut ();
			print_wb b;
			print_string ",";
				open_vbox 1;
				print_space ();
				print_ws s;
				close_box ();
			print_string ")";
			close_box ();
		end
	| Block(vd, pd, s)	-> begin
			print_string "Block(";
			open_vbox 1;
			print_cut ();
			print_vardecs vd;
			print_string ",";
			print_space ();
			print_procdecs pd;
			print_string ",";
			print_space ();
			print_ws s;
			print_string ")";
			close_box ();
		end
	| Call(pname, oa)	-> begin
			print_string ("Call(" ^ pname ^ ",");
			print_space ();
			(match oa with
				| None 		-> print_string "None"
				| Some a 	-> print_wa a);
			print_string ")";
		end
	| Write(lbl)		-> print_string ("Write(" ^ lbl ^ ")")
	| Raise(lbl)		-> print_string ("Raise(" ^ lbl ^ ")")
	| Try(s,c)			-> begin
			print_string "Try(";
			print_ws s;
			print_string ",";
			print_space ();
			print_catchers c;
			print_string ")";
		end
	| Read(lbl)			-> print_string ("Read(" ^ lbl ^ ")")
and print_vardecs vd = match vd with
	| DecVarNone			-> print_string "DecVarNone"
	| DecVar(lbl,a,rest)	-> begin
			open_box 1;
				print_string ("DecVar(" ^ lbl ^ ",");
				print_space ();
				print_wa a;
				print_string ",";
				print_space ();
			close_box ();
			print_vardecs rest;
		end
and print_procdecs pd = match pd with
	| DecProcNone				-> print_string "DecProcNone";
	| DecProc(lbl,arg,s,rest) 	-> begin
			open_box 1;
				print_string ("DecProc(" ^ lbl ^ ",");
				print_space ();
				print_option arg;
				print_string ",";
				print_space ();
				print_ws s;
				print_string ",";
				print_space ();
			close_box ();
			print_procdecs rest;
		end
and print_option x = match x with
	| None 		-> print_string "None"
	| Some(v)	-> print_string ("Some(" ^ v ^ ")")
and print_catchers cs = match cs with
	| RescueNone			-> print_string "RescueNone"
	| Rescue(lbl,s,rest)	-> begin
			open_box 1;
				print_string ("Rescue(" ^ lbl ^ ",");
				print_space ();
				print_ws s;
				print_string ",";
				print_space ();
			close_box ();
			print_catchers rest;
		end
and print_wa a = match a with
	| Constant(n)	-> print_string ("Constant(" ^ (string_of_int n) ^ ")")
	| Var(lbl)		-> print_string ("Var(" ^ lbl ^ ")")
	| Add(a1,a2)	-> begin
			open_box 1;
			print_string "Add(";
			print_wa a1;
			print_string ",";
			print_space ();
			print_wa a2;
			print_string ")";
			close_box ();
		end
	| Mul(a1,a2)	-> begin
			open_box 1;
			print_string "Mul(";
			print_wa a1;
			print_string ",";
			print_space ();
			print_wa a2;
			print_string ")";
			close_box ();
		end
	| Sub(a1,a2)	-> begin
			open_box 1;
			print_string "Sub(";
			print_wa a1;
			print_string ",";
			print_space ();
			print_wa a2;
			print_string ")";
			close_box ();
		end
	| Neg(a0)		-> begin
			open_box 1;
			print_string "Neg(";
			print_wa a0;
			print_string ")";
			close_box ();
		end
and print_wb b = match b with
	| TrueValue			-> print_string "TrueValue"
	| FalseValue		-> print_string "TrueValue"
	| Equal(a1,a2)		-> begin
			open_box 1;
			print_string "Equal(";
			print_wa a1;
			print_string ",";
			print_space ();
			print_wa a2;
			print_string ")";
			close_box ();
		end
	| Leq(a1,a2)		-> begin
			open_box 1;
			print_string "Leq(";
			print_wa a1;
			print_string ",";
			print_space ();
			print_wa a2;
			print_string ")";
			close_box ();
		end
	| Not(b0)			-> begin
			open_box 1;
			print_string "Not(";
			print_wb b0;
			print_string ")";
			close_box ();
		end
	| And(b1,b2)		-> begin
			open_box 1;
			print_string "And(";
			print_wb b1;
			print_string ",";
			print_space ();
			print_wb b2;
			print_string ")";
			close_box ();
		end

let print_ast ast = begin
		print_ws ast;
		print_newline ();
	end
