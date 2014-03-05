open Wast


(*
  ==============================
  ===== Type definitions   =====
  ==============================
*)

type 	env_prog  = env_v * env_p * store
and   env_v = (identifier * int) list
and   env_p = EnvP of ((identifier * ((identifier option) * ws * env_v * env_p)) list)
and   store = {
  data : data_type array;
  mutable next : int;           (* next free place in store *)
  mutable exc : string option;  (* Currently raised exception *)
}

exception RuntimeError

(*
  ==============================
  ===== Env aux functions  =====
  ==============================
*)

(* Finishing program with runtime error *)
let runtime_error str = begin
    Printf.printf "%s\n" str;
    raise RuntimeError
  end

(* Clean environment *)
let clean_env =
  let sto = {
    data = Array.make 1000 0;
    next = 0;
    exc = None;
  } in (([], EnvP([]), sto), sto)

(* Computing new 'next' field in store *)
let new_next x = x + 1

(* Synonym for unit *)
let nothing = ()

(* Check if option is Some constructor *)
let is_some = function | None -> false | Some _ -> true

(* Fetch var value from env *)
let get_var_val env lbl =
  let (envV, _, sto) = env
  in try
    let loc = List.assoc lbl envV in sto.data.(loc)
  with Not_found ->
    Printf.ksprintf runtime_error "Variable `%s` is not defined." lbl

(* Set var value in env *)
let set_var_val env lbl res = 
  let (envV, _, sto) = env
  in try
    let loc = List.assoc lbl envV in sto.data.(loc) <- res
  with Not_found ->
    Printf.ksprintf runtime_error "Variable `%s` is not defined." lbl

(* Fetch proc's arg name, code and env *)
let get_proc_env env pname =
  let (envV, EnvP(envP_list), sto) = env
  in let (vname, pstm, penvV, penvP) = List.assoc pname envP_list
  in (vname, pstm, (penvV, penvP, sto))

(* Printing in Write statement *)
let puts env lbl =
  let res = get_var_val env lbl
  in Printf.printf "%s = %d\n%!" lbl res

(* Reading in Read statement *)
let gets env lbl =
  begin
    Printf.printf "? %s = %!" lbl;
    Scanf.scanf "%d" (set_var_val env lbl);
  end

(* Fetch raised exception. Fails if exception isn't set *)
let raised_exception env =
  let (_, _, sto) = env in match sto.exc with
    | None        -> failwith "Error: There is not raised exception." 
    | Some(name)  -> name

(* Check if there is raised exception *)
let is_raised_exception env =
  let (_, _, sto) = env in is_some sto.exc

(* Set exception name *)
let set_exception env lbl =
  let (_, _, sto) = env in (sto.exc <- (Some(lbl)))

(* Unset exception *)
let unset_exception env = 
  let (_, _, sto) = env in (sto.exc <- None)

(* Storage dump *)
let display_storage sto = 
  begin
    Printf.printf "=========================\n";
    Printf.printf "===== Storage dump: =====\n";
    for i = 0 to (sto.next - 1) do
      let res = sto.data.(i) in
        Printf.printf "%d:\t\t%d\n" i res;
    done
  end

(* Reporting an exception *)
let report_exception = function
  | None      -> nothing
  | Some exc  -> Printf.ksprintf runtime_error "Program ended with exception: %s." exc

(* Fetch proc from env *)
let get_proc env pname = 
  let (_, (EnvP(envP_list)), _) = env
  in try List.assoc pname envP_list
  with Not_found ->
    Printf.ksprintf runtime_error "Procedure `%s` is not defined." pname

(*
  ==============================
  ===== Interpreter        =====
  ==============================
*)


(* Reserving new var in var env *)
let rec env_with_new_var env lbl aexp = 
  let (envV, envP, sto) = env
  in let l = sto.next
  in let res = compute_a env aexp
  in let envV' = (lbl, l) :: envV
  in begin 
    sto.data.(l) <- res;
    sto.next <- new_next l;
    (envV', envP, sto)
  end

(* Reserving new procedure in proc env *)
and env_with_new_proc env pname vname stm = 
  let (envV, envP, sto) = env
  in let EnvP(envP_list) = envP
  in let envP' = EnvP((pname, (vname, stm, envV, envP)) :: envP_list)
  in (envV, envP', sto)

(* Prepare environment (ex. compute value of argument) for running proc *)
and prepare_env env nameOpt aexpOpt pname = match (nameOpt, aexpOpt) with
  | (None, None)            -> env
  | (Some _, None)          -> Printf.ksprintf runtime_error "Error: Not enough arguments given to function %s." pname
  | (None, Some _)          -> Printf.ksprintf runtime_error "Error: Too many arguments given to function %s." pname
  | (Some name, Some aexp)  -> env_with_new_var env name aexp

(* Constructing env with var env from declarations in block *)
and exc_var_decs env var_decs = match var_decs with
  | DecVar(lbl, aexp, tail)   ->
      exc_var_decs (env_with_new_var env lbl aexp) tail
  | DecVarNone                -> env

(* Constructing env with proc env from declarations in block *)
and exc_proc_decs env proc_decs = match proc_decs with
  | DecProc(pname, vname, ws, tail)   ->
      exc_proc_decs (env_with_new_proc env pname vname ws) tail
  | DecProcNone                       -> env

(* Computing value of aexp based on AST *)
and compute_a env wa = match wa with
  | Constant(n)   -> n
  | Neg(a)        -> - (compute_a env a)
  | Var(lbl)      -> get_var_val env lbl
  | Add(a1,a2)    -> (compute_a env a1) + (compute_a env a2)
  | Mul(a1,a2)    -> (compute_a env a1) * (compute_a env a2)
  | Sub(a1,a2)    -> (compute_a env a1) - (compute_a env a2)

(* Computing value of bexp based on AST *)
and compute_b env wb = match wb with
  | TrueValue     -> true
  | FalseValue    -> false
  | Equal(a1,a2)  -> (compute_a env a1) = (compute_a env a2)
  | Leq(a1,a2)    -> (compute_a env a1) <= (compute_a env a2)
  | Not(b)        -> not (compute_b env b)
  | And(b1,b2)    -> (compute_b env b1) && (compute_b env b2)

(* Executing code *)
and execute env stm = match stm with
  | Skip              -> ()
  | Assgn(lbl, wa)    -> set_var_val env lbl (compute_a env wa)
  | Cons(s1, s2)      ->
      begin
        execute env s1;
        if is_raised_exception env
        then nothing
        else execute env s2
      end
  | Cond(b,s1,s2)     ->
      if   compute_b env b
      then execute env s1
      else execute env s2
  | While(b,s)        -> execute env (Cond(b, Cons(s,stm), Skip))
  | Block(var_decs, proc_decs, stm) ->
      let env_with_vars      = exc_var_decs env var_decs
      in let env_with_procs  = exc_proc_decs env_with_vars proc_decs
      in execute env_with_procs stm
  | Write(lbl)        -> puts env lbl
  | Try(s, catch)     ->
      begin
        execute env s;
        if is_raised_exception env
        then rescue_exception env catch
        else nothing
      end
  | Read(lbl)         -> gets env lbl
  | Call(pname,wa)    ->
      let (_, _, sto) = env
      in let current_next = sto.next
      in let (vname, stm, penvV, penvP) = get_proc env pname
      in let env_with_rec_proc = env_with_new_proc (penvV, penvP, sto) pname vname stm
      in let env_with_argument = prepare_env env_with_rec_proc vname wa pname
      in begin
        execute env_with_argument stm;
        sto.next <- current_next;
      end
  | Raise(lbl)        -> set_exception env lbl

(* Rescuing the program from the exception *)
and rescue_exception env catch = match catch with
  | RescueNone        -> nothing
  | Rescue(lbl,s,rst) ->
    if lbl <> raised_exception env
    then rescue_exception env rst
    else begin
      unset_exception env;
      execute env s
    end

(*
  ==============================
  ===== Main run function  =====
  ==============================
*)

let run ast =
  let (env, sto) = clean_env
  in try begin
    execute env ast;
    report_exception sto.exc;
    sto
  end with RuntimeError -> begin
    Printf.printf "Program finished unexpectedly.\n";
    sto
  end