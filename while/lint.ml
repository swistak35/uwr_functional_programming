open Wast

(*
  ==============================
  ===== Type definitions   =====
  ==============================
*)

type msg = 
  | AlreadyUsedVarName of identifier
  | AlreadyUsedProcName of identifier
  | NonDefinedVarUsed of identifier
  | NonDefinedProcUsed of identifier
  | NotEnoughArguments of identifier * identifier
  | TooManyArguments of identifier
  | NotRescuedException of identifier

type  env_prog  = env_v * env_p * (msg list ref)
and   env_v = identifier list
and   env_p = EnvP of ((identifier * ((identifier option) * ws * env_v * env_p)) list)

(*
  ==============================
  ===== Env aux functions  =====
  ==============================
*)

(* Clean environment *)
let clean_env : env_prog * (msg list ref) =
  let msgs = ref []
  in (([], EnvP([]), msgs), msgs)

(* Synonym for unit *)
let nothing = ()

(* Check if option is Some constructor *)
let is_some = function | None -> false | Some _ -> true

(* Get value of an option *)
let get_some = function | None -> failwith "Unexpected None" | Some x -> x

(* Delete element from the list, only first occurrence *)
let rec delete lst x = match lst with
  | []    -> []
  | h::t  ->
      if h = x
      then t
      else h :: (delete t x)

(* Make an unique list *)
let uniq lst = 
  let rec _uniq lst acc = match lst with
    | []    -> []
    | h::t  ->
      if List.mem h acc
      then _uniq t acc
      else h :: (_uniq t (h::acc))
  in _uniq lst []

(* 'Getters' for env_prog *)
let vars env  = let (vars, _, _)        = env in vars
let procs env = let (_, EnvP(procs), _) = env in procs
let msgs env  = let (_, _, msgs)        = env in msgs

let defined_procs env = fst (List.split (procs env))

(*
  Adding new msg
  Note: I'm aware that using mutable list is weird in that case, but
    it saved me a lot of "let ... in ..." statements in the whole checker.
*)
let msg_add env msg =
  let msgs = (msgs env)
  in msgs := msg :: !msgs

(* Returns env with new var in envV *)
let env_with_new_var env lbl = 
  let (envV, envP, msgs) = env
  in (lbl :: envV, envP, msgs)

(* Returns env with new proc in envP *)
let env_with_new_proc env pname vname stm = 
  let (envV, envP, msgs) = env
  in let EnvP(envP_list) = envP
  in let envP' = EnvP((pname, (vname, stm, envV, envP)) :: envP_list)
  in (envV, envP', msgs)

(* Check if used var is defined *)
let check_var_existence env lbl =
  if List.mem lbl (vars env)
  then nothing
  else msg_add env (NonDefinedVarUsed lbl)

(* Check if used var is overriden *)
let check_var_availability env lbl =
  if List.mem lbl (vars env)
  then msg_add env (AlreadyUsedVarName lbl)
  else nothing

(* Check if used proc is overriden *)
let check_proc_availability env lbl = 
  if List.mem lbl (defined_procs env)
  then msg_add env (AlreadyUsedProcName lbl)
  else nothing

(* Collecting all exceptions which may raise in statement *)
let rec possible_exceptions ws =
  let rec find_in_stm ws = match ws with
    | Cons(s1, s2)      -> (find_in_stm s1) @ (find_in_stm s2)
    | Cond(b,s1,s2)     -> (find_in_stm s1) @ (find_in_stm s2)
    | While(b,s)        -> find_in_stm s
    | Block(_, proc_decs, stm) ->
        (find_in_stm stm) @ (find_in_proc_decs proc_decs)
    | Try(s, catch)     -> find_in_stm s
    | Raise(lbl)        -> [lbl]
    | _                 -> []
  and find_in_proc_decs proc_decs = match proc_decs with
    | DecProc(_, _, ws, tail)   ->
        (find_in_stm ws) @ (find_in_proc_decs tail)
    | DecProcNone               -> []
  in find_in_stm ws



(*
  ==============================
  ===== Code checker       =====
  ==============================
*)

(* Checking statements *)
let rec stm_check env stm = match stm with
  | Skip              -> nothing
  | Assgn(lbl, wa)    -> begin
        check_var_existence env lbl;
        wa_check env wa;
      end
  | Cons(s1, s2)      -> begin
        stm_check env s1;
        stm_check env s2;
      end
  | Cond(b,s1,s2)     -> begin
        wb_check env b;
        stm_check env s1;
        stm_check env s2;
      end
  | While(b,s)        -> begin
        wb_check env b;
        stm_check env s;
      end
  | Block(var_decs, proc_decs, stm) -> 
      let env_with_vars      = var_decs_check env var_decs
      in let env_with_procs  = proc_decs_check env_with_vars proc_decs
      in stm_check env_with_procs stm
  | Write(lbl)        -> check_var_existence env lbl
  | Try(s, catch)     -> begin
        stm_check env s;
        catch_check env catch (possible_exceptions s);
      end
  | Read(lbl)         -> check_var_existence env lbl
  | Call(pname,wa)    -> check_proc_call env pname wa
  | Raise(lbl)        -> nothing

(* Checking arithmetic expressions *)
and wa_check env wa = match wa with
  | Constant(n)   -> nothing
  | Neg(a)        -> wa_check env a
  | Var(lbl)      -> check_var_existence env lbl
  | Add(a1,a2)    -> double_wa_check env a1 a2
  | Mul(a1,a2)    -> double_wa_check env a1 a2
  | Sub(a1,a2)    -> double_wa_check env a1 a2

(* Checking boolean expressions *)
and wb_check env wb = match wb with
  | TrueValue     -> nothing
  | FalseValue    -> nothing
  | Equal(a1,a2)  -> double_wa_check env a1 a2
  | Leq(a1,a2)    -> double_wa_check env a1 a2
  | Not(b)        -> wb_check env b
  | And(b1,b2)    -> double_wb_check env b1 b2

(* Checking code in rescue blocks and finding unhandled exceptions *)
and catch_check env catch excs = 
  let rec _catch_check env catch excs = match catch with
    | RescueNone        ->
        ignore (List.map
          (fun e -> msg_add env (NotRescuedException e)) excs)
    | Rescue(lbl,s,rst) -> begin
          stm_check env s;
          _catch_check env rst (delete excs lbl);
        end
  in _catch_check env catch (uniq excs)

(* Checking var declarations in blocks, returns new env *)
and var_decs_check env var_decs = match var_decs with
  | DecVar(lbl, aexp, tail)   -> begin
        check_var_availability env lbl;
        wa_check env aexp;
        var_decs_check (env_with_new_var env lbl) tail
      end
  | DecVarNone                -> env

(* Checking proc declarations in blocks, returns new env *)
and proc_decs_check env proc_decs = match proc_decs with
  | DecProc(pname, vname, ws, tail)   -> begin
        check_proc_availability env pname;
        if is_some vname
          then check_var_availability env (get_some vname);
        check_proc_declaration env pname vname ws;
        proc_decs_check (env_with_new_proc env pname vname ws) tail
      end
  | DecProcNone                       -> env

(* Check proc statement *)
and check_proc_declaration env pname vnameOpt ws = 
  let env_with_proc = (env_with_new_proc env pname vnameOpt ws)
  in let (envV, envP, msgs) = env_with_proc
  in match vnameOpt with
    | None        -> stm_check env_with_proc ws
    | Some(vname) -> stm_check (vname :: envV, envP, msgs) ws

(* Checking proc call *)
and check_proc_call env pname wa = 
  if List.mem pname (defined_procs env)
  then check_proc_argument env pname wa
  else msg_add env (NonDefinedProcUsed pname)

(* Checking if arguments are proper in procedure call *)
and check_proc_argument env pname aexpOpt = 
  let (nameOpt, pstm, penvV, penvP) = List.assoc pname (procs env)
  in match (nameOpt, aexpOpt) with
    | (None, None)            -> nothing
    | (Some name, None)       -> msg_add env (NotEnoughArguments(pname, name))
    | (None, Some aexp)       -> msg_add env (TooManyArguments pname)
    | (Some name, Some aexp)  -> wa_check env aexp

(* Auxiliary functions *)
and double_wa_check env x1 x2 = begin
    wa_check env x1;
    wa_check env x2;
  end
and double_wb_check env x1 x2 = begin
    wb_check env x1;
    wb_check env x2;
  end



(*
  ==============================
  ===== Main check func    =====
  ==============================
*)

let check ast =
  let (env, msgs) = clean_env
  in begin
    stm_check env ast;
    msgs
  end

let print msgs = 
  let show = Printf.printf
  in let _print msg = match msg with
    | AlreadyUsedVarName(name)    -> show "Warning: Variable `%s` is overriden.\n" name
    | AlreadyUsedProcName(name)   -> show "Warning: Procedure `%s` is overriden.\n" name
    | NonDefinedVarUsed(name)     -> show "Error: Variable `%s` is not defined.\n" name
    | NonDefinedProcUsed(name)    -> show "Error: Procedure `%s` is not defined.\n" name
    | NotEnoughArguments(name,arg)-> show "Error: In `%s` call, argument `%s` is missing.\n" name arg
    | TooManyArguments(name)      -> show "Error: In `%s` call, there are too many arguments.\n" name
    | NotRescuedException(name)   -> show "Warning: Exception `%s` is not rescued.\n" name
  in ignore (List.map _print (List.rev msgs))