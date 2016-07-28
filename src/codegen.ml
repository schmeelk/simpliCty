(*
Project:  COMS S4115, SimpliCty Compiler
Filename: src/codegen.ml
Authors:  - Rui Gu,           rg2970
          - Adam Hadar,       anh2130
          - Zachary Moffitt,  znm2104
          - Suzanna Schmeelk, ss4648
Purpose:  * Translates semantically checked SimpliCty AST to LLVM IR
          * Functions for printing the AST
Modified: 2016-07-25
*)
(*: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "SimpliCty"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let ltype_of_typ = function
      A.Int  -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t
  in
  let primary_decompose = function
      A.Literal(i) -> i
    | A.BoolLit(b) -> if b then 1 else 0
    | _            -> 0
  in
  
  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (typ, name, _, _, _, _) =
          let init = L.const_int (ltype_of_typ typ) 0
          in StringMap.add name (L.define_global name init the_module) m
      
      in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare putchar(), which the putchar built-in function will call *)
  let putchar_t = L.function_type i32_t [| i32_t |] in
  let putchar_func = L.declare_function "putchar" putchar_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_, _, _) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in    
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (typ, name, decl, size) p =
        (match decl with
          A.Primitive ->
            L.set_value_name name p;
	    let local = L.build_alloca (ltype_of_typ typ) name builder in
	    ignore (L.build_store p local builder);
	    StringMap.add name local m
        | A.Array -> L.set_value_name name p;
            let size_s = (match size with
              A.Primary(A.Literal(s)) -> L.const_int i32_t s
            | _ -> L.const_int i32_t 0) in
	    let local = L.build_array_alloca (ltype_of_typ typ) size_s name builder in
	    ignore (L.build_store p local builder);
	    StringMap.add name local m
        ) in

      let assign_primitive addr typ' assign value =
        L.build_store (L.const_int typ' (match assign with
          A.DeclAssnYes -> (match value with
            [p] -> primary_decompose p
          | _   -> 0)
        | _             -> 0)
        ) addr builder
      and assign_array addr typ' assign values = 
        List.fold_left (fun index _vals ->
          (let i    = L.const_int i32_t index
          and value = L.const_int typ' (match assign with
            A.DeclAssnYes -> primary_decompose _vals
          | _             -> 0)
          in
          let addr' = L.build_in_bounds_gep addr [|i|] "tmp" builder in
          ignore(L.build_store value addr' builder); index+1)
      ) 0 values
      in
      let add_local m (typ, name, decl, size, assign, values) =
        let typ' = ltype_of_typ typ in
        let addr = (match decl with
          A.Primitive -> L.build_alloca typ'
        | A.Array     -> L.build_array_alloca typ' (match size with
            A.Primary(A.Literal(s)) -> L.const_int typ' s
          | _                       -> L.const_int i32_t 0)
        ) name builder in
        (match decl with
          A.Primitive -> ignore(assign_primitive addr typ' assign values)
        | A.Array     -> ignore(assign_array     addr typ' assign values)
        ); StringMap.add name addr m
      in
      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in


    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in
    (*Construct code for lvalues; return value pointed to*)
    let lvalue builder = function
      A.Id(s)    -> L.build_load (lookup s) s builder
    | A.Arr(s,i) ->
        let s' = lookup s
        and i' = L.const_int i32_t i in
        let addr = L.build_in_bounds_gep s' [|i'|] "tmp" builder in
        L.build_load addr s builder
    in
    (*Construct code for literal primary values; return its value*)
    let primary builder = function
      A.Literal i -> L.const_int i32_t i
    | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | A.Lvalue lv -> lvalue builder lv            
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.Primary p -> primary builder p
      | A.Noexpr -> L.const_int i32_t 0
      | A.Binop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_srem
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
          let e' = expr builder e in
	  (match op with
	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | A.Crement(opDir, op, lv) ->
          (match opDir with
            A.Pre  -> expr builder (A.Assign(lv, (match op with
              A.PlusPlus   -> A.AssnAdd
            | A.MinusMinus -> A.AssnSub), (A.Primary (A.Literal 1))))
          | A.Post ->
              let lv' = lvalue builder lv in
             ignore(expr builder (A.Crement(A.Pre, op, lv))); lv'
          )
      | A.Assign (lv, op, e) ->
          let lv'  = (A.Primary (A.Lvalue lv))
          and lv'' = (match lv with
            A.Id(s)    -> lookup s
          | A.Arr(s,i) ->
              let s' = lookup s
              and i' = L.const_int i32_t i in
              L.build_in_bounds_gep s' [|i'|] "tmp" builder) in
          let e' = (match op with
            A.AssnReg     -> expr builder e
          | A.AssnAdd     -> expr builder (A.Binop(lv', A.Add,  e))
          | A.AssnSub     -> expr builder (A.Binop(lv', A.Sub,  e))
          | A.AssnMult    -> expr builder (A.Binop(lv', A.Mult, e))
          | A.AssnDiv     -> expr builder (A.Binop(lv', A.Div,  e))
          | A.AssnMod     -> expr builder (A.Binop(lv', A.Mod,  e))
          ) in
          ignore (L.build_store e' lv'' builder); e'
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | A.Call ("putchar", [e]) ->
      L.build_call putchar_func [| (expr builder e) |]
        "putchar" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
	  A.Void -> L.build_ret_void builder
	| _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore (L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
