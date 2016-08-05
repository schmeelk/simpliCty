(*
Project:  COMS S4115, SimpliCty Compiler
Filename: src/simplicty.ml
Authors:  - Rui Gu,           rg2970
          - Adam Hadar,       anh2130
          - Zachary Moffitt,  znm2104
          - Suzanna Schmeelk, ss4648
Purpose:  * Top level for SimpliCty compiler
            * Scan & parse input, global variables
            * Check each function in the resulting AST, generate LLVM IR, dump module
Modified: 2016-07-24
*)

type action = Ast | LLVM_IR | Compile

let _ =
  if Array.length Sys.argv <= 1 then (
    print_string "Usage: ./simplicty [-a|-l|-c] source_code\n";
    exit 1);;
  let action = if Array.length Sys.argv > 2 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);	(* Print the AST only *)
			      ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
			      ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  let inputfile = Sys.argv.(Array.length Sys.argv - 1) in
  let lexbuf = Lexing.from_channel (open_in inputfile) in
  let ast = Parser.program Scanner.token lexbuf in
  Semant.check ast;
  match action with
    Ast -> print_string (Ast.string_of_program ast)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
  | Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
