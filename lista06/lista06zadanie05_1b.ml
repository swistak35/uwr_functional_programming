(* prolog.ml *)

(* An atom is either a propositional variable or an alternative of two goals. *)
type atom = 
  | Atom of string 
  | Or of goal * goal
(* A goal is a list of atoms. *)
and goal = atom list
(* A clause consists of a head (a propositional variable) and a body (a goal). *)
type clause = string * goal
(* A Prolog program is a list of clauses. *)
type program = clause list

(* Search a program for a clause with a given head. *)    
let rec lookup x pgm =
  match pgm with
    | [] ->
      None
    | (y, g) :: p ->
      if x = y then Some g else lookup x p

(* 
A propositional Prolog interpreter written in CPS with two layers of continuations: 
a success and a failure continuation. The failure continuation is parameterless and 
it specifies what should happen next in case of a failure in the current goal. The 
success continuation takes a failure continuation as an argument and it specifies 
what should happen next in case the current goal is satisfied. 
*)
	
(*      eval_atom : atom -> program -> ((unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a *)
let rec eval_atom a p sc fc n =
  match a with
    | Atom x ->
      (match (lookup x p) with
	| None -> 
	  fc n
	| Some g -> 
	  eval_goal g p sc fc n)
    | Or (g1, g2) ->
      eval_goal g1 p sc (fun k -> eval_goal g2 p sc fc k) n

(*  eval_goal : goal -> program -> ((unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a  *)
and eval_goal g p sc fc n =
  match g with
    | [] -> 
      sc fc n
    | a :: g -> 
      eval_atom a p (fun fc' k -> eval_goal g p sc fc' k) fc n

(*  run : goal ->  program -> bool  *)
let run g p = eval_goal g p (fun f n -> f (n+1)) (fun n -> n) 0

(*     Wyjaśnienie:
  Dodatkowy argumenty w eval_goal i kontynuacja 'id' dla fc jest oczywisty
  Funkcja z sukcesem, za każdym razem kiedy ten sukces osiągnie, próbuje się
  spełnić na pozostałe sposoby, jednak do licznika "ile już mamy sukcesów"
  dodaje 1, i ta liczba zostanie na końcu zwrócona przez wykonanie
  początkowej funkcji fc. W pozostałych przypadkach eval_goal i eval_atom
  tylko propagujemy tą wartość dalej, bo jedynym miejscem, gdzie chcemy ten licznik
  zmienić, jest sukces.
*)

(* tests *)
  
let p1 = [("a", [Atom "b"; Atom "c"]);
	  ("b", [])]
  
let p2 = [("a", [Atom "b"; Or ([Atom "c"], [Atom "d"]); Atom "e"]);
	  ("b", [Atom "d"]);
	  ("d", []);
	  ("e", [Atom "d"])]
  
let p3 = [("a", [Atom "b"; Or ([Atom "c"], [Atom "d"]); Atom "e"]);
	  ("b", [Atom "d"]);
	  ("c", []);
	  ("d", []);
	  ("e", [Atom "d"])]
let p4 = [("a", [Atom "b"]);
          ("b", [Atom "c"]);
          ("c", [Atom "b"])]
  
let g1 = [Atom "a"] 

let v1_1 = run g1 p1
let v1_2 = run g1 p2
let v1_3 = run g1 p3  
(* let v1_4 = run g1 p4 *)  

(* eof *)