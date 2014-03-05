let runner_interpreter cin options = 
  let display_sto = List.mem "--dump" options in
  let lexbuf = Lexing.from_channel cin in
      begin
        let ast = Wparser.input Wlexer.token lexbuf in
        let sto = Interpreter.run ast in
        begin
          if display_sto then Interpreter.display_storage sto;
        end
      end

let runner_vm_compiler cin options = 
  Printf.printf "This feature isn't supported yet."

let runner_static_analysis cin options = 
  let lexbuf = Lexing.from_channel cin
  in let ast = Wparser.input Wlexer.token lexbuf
  in let msgs = !(Lint.check ast)
  in begin
    Printf.printf "%d errors/warnings found.\n" (List.length msgs);
    Lint.print msgs
  end

let runner_pretty_printer cin options = 
  let lexbuf = Lexing.from_channel cin
  in let ast = Wparser.input Wlexer.token lexbuf
  in Printer.print_ast ast

let main () =
  if (Array.length Sys.argv) < 3
  then Printf.printf "Not enough arguments.\n"
  else
    let runner = Sys.argv.(1) in
    let cin = open_in Sys.argv.(2) in
    let argstl = List.tl (List.tl (List.tl (Array.to_list Sys.argv))) in
      match runner with
        | "interpret"   -> runner_interpreter cin argstl
        | "compile"     -> runner_vm_compiler cin argstl
        | "check"       -> runner_static_analysis cin argstl
        | "ast"         -> runner_pretty_printer cin argstl
        | _             -> Printf.printf "Wrong runner.\n"

let _ = main ()
