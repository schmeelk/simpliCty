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

let translate (globals, externs, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "SimpliCty"
  and i32_t  = L.i32_type   context
  and f32_t  = L.float_type context
  and i1_t   = L.i1_type    context
  and void_t = L.void_type  context in

  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> f32_t
    | A.Char  -> i32_t
    | A.String -> i32_t
    | A.Bool  -> i1_t
    | A.Void  -> void_t
  in
  let primary_decompose = function 
      A.IntLit(i)     -> i 
    | A.BoolLit(b)    -> if b then 1 else 0
    | A.FloatLit(f)   -> int_of_float f
    | _               -> 0

  and primary_float_decompose = function
      A.IntLit(i)   -> float_of_int i
    | A.BoolLit(b)  -> if b then 1.0 else 0.0
    | A.FloatLit(f) -> f
    | _             -> 0.0

  in
 
  (* Store memory *) 
  let store_primitive addr typ' assign value builder =
    L.build_store (L.const_int typ' (match assign with
      A.DeclAssnYes -> (match value with
        [p] -> primary_decompose p
      | _   -> 0)
    | _             -> 0)
    ) addr builder
  and store_array_idx addr index typ' assign value builder =
    let i  = L.const_int i32_t index
    and v' = L.const_int typ' (match assign with
      A.DeclAssnYes -> primary_decompose value
    | _             -> 0)
    in
    let addr' = L.build_in_bounds_gep addr [|i|] "tmp" builder in
    L.build_store v' addr' builder
  and store_float_primitive addr typ' assign value builder =
    L.build_store (L.const_float typ' (match assign with
      A.DeclAssnYes -> (match value with
        [p] -> primary_float_decompose p
      | _   -> 0.0)
    | _             -> 0.0)
    ) addr builder

  in
 
  (* Declare each global variable; remember its value in a map *)
  (*TODO-ADAM: global scoped arrays*)
  let global_vars =
    let global_var m (typ, name, _, _, assign, values) =
      let typ' = ltype_of_typ typ in
      let init = (match typ with
        A.Float -> L.const_float typ' (match assign with A.DeclAssnYes -> (match values with [p] -> primary_float_decompose p | _ -> 0.0) | _ -> 0.0)
      | _       -> L.const_int   typ' (match assign with A.DeclAssnYes -> (match values with [p] -> primary_decompose p | _ -> 0) | _ -> 0)
      ) in
      let addr = L.define_global name init the_module in
      StringMap.add name (addr, A.Primitive, 0) m   
    in
    List.fold_left global_var StringMap.empty globals in

  (* Declare putchar(), which the putchar built-in function will call *)
  let putchar_t = L.function_type i32_t [| i32_t |] in
  let putchar_func = L.declare_function "putchar" putchar_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types = 
	Array.of_list (List.map
          (fun (t,_, decl, _) ->
             (match decl with
               A.Primitive -> ltype_of_typ t
             | A.Array -> L.pointer_type (ltype_of_typ t)))
        fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  let extern_decls = List.fold_left (fun ed e ->
     { A.typ = e.A.e_typ; A.fname = e.A.e_fname; A.formals = e.A.e_formals;
       A.locals = []; A.body = [] } :: ed)
                         [] externs
  in

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (t,_, _, _) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.declare_function name ftype the_module, fdecl) m in
    List.fold_left function_decl function_decls extern_decls in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (typ, name, decl, size) p =
        L.set_value_name name p;
        let typ' = ltype_of_typ typ
        and size' = L.const_int i32_t size in
        let addr = (match decl with
          A.Primitive -> L.build_alloca typ'
        | A.Array     -> L.build_array_alloca typ' size') name builder in
        (match decl with
          A.Primitive ->
	    ignore(L.build_store p addr builder)
        | A.Array ->
            (*TODO-ADAM: pass full arrays?*)
            let rec arrFormal idx =
            (match idx with -1 -> 0
            | _ ->
              let new_addr = L.build_in_bounds_gep addr [|L.const_int i32_t idx|] "new" builder
              and old_addr = L.build_in_bounds_gep p    [|L.const_int i32_t idx|] "old" builder in
              let old_val = L.build_load old_addr "oldV" builder in
              ignore(L.build_store old_val new_addr builder); arrFormal (idx-1)
            ) in ignore(arrFormal (size-1))); StringMap.add name (addr,decl,size) m
      in
      let add_local m (typ, name, decl, size, assign, values) =
        let typ' = ltype_of_typ typ
        and size' = L.const_int i32_t size in
        let addr = (match decl with
          A.Primitive -> L.build_alloca typ'
        | A.Array     -> L.build_array_alloca typ' size') name builder in
        (match decl with
          A.Primitive -> (match typ with 
             A.Float -> ignore(store_float_primitive addr typ' assign values builder)
            | _ ->  ignore(store_primitive addr typ' assign values builder))
        | A.Array     ->
            ignore(List.fold_left (fun index _vals ->
            ignore(store_array_idx addr index typ' assign _vals builder);index+1) 0 values)
        ); StringMap.add name (addr,decl,size) m
      in
      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in


    (* Return the value for a variable or formal argument *)
    let lookup_addr n = 
      (fun (a,_,_) -> a)
      (try StringMap.find n local_vars
         with Not_found -> StringMap.find n global_vars)
    and lookup_decl n =
      (fun (_,b,_) -> b)
      (try StringMap.find n local_vars
         with Not_found -> StringMap.find n global_vars)
    and lookup_size n =
      let (_,_,c) =
      (try StringMap.find n local_vars
         with Not_found -> StringMap.find n global_vars)
      in c
    in
    (*Construct code for lvalues; return value pointed to*)
    
    let lvalue builder = function
      A.Id(s)    ->
         let addr = lookup_addr s
         and decl = lookup_decl s in
         (match decl with
           A.Primitive -> 
            (addr, decl, 0,0)
         | A.Array     ->
            (addr, decl, (lookup_size s),1)
         )
    | A.Arr(s,i) ->
        let s' = lookup_addr s
        and decl = lookup_decl s
        and i' = L.const_int i32_t i in
        let addr = L.build_in_bounds_gep s' [|i'|] "arr" builder in
        (addr, decl, 0,0)
    in    
 
    let primary builder = function
      A.IntLit i   -> (L.const_int i32_t i                       , A.Primitive, 0)
    | A.FloatLit f -> (L.const_float f32_t f                     , A.Primitive, 0)
    | A.CharLit c  -> (L.const_int i32_t (int_of_char c)         , A.Primitive, 0)
    | A.BoolLit b  -> (L.const_int i1_t (if b then 1 else 0)     , A.Primitive, 0)
    | A.Lvalue lv  ->
        let (value, decl, size,is_arr) = lvalue builder lv in
        (match decl with
          A.Primitive -> (L.build_load value "lv" builder, decl, size)
        | A.Array     -> if is_arr = 1
            then (value, decl, size)
            else (L.build_load value "lv" builder, decl, size))
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.Primary p -> primary builder p
      | A.Noexpr -> (L.const_int i32_t 0, A.Primitive, 0)
      | A.StringConv p -> primary builder (A.CharLit p)
      | A.ListCreate p -> (L.const_int i32_t (List.length(p)), A.Primitive, 0)  
      | A.Binop (e1, op, e2) ->
          let e1' = match (expr builder e1) with
            (c , A.Primitive,_) -> c
          | (p , _,_)           -> L.const_inttoptr p (L.pointer_type i32_t)
            (*TODO-ADAM: this is a dummy for array math*)
	  and e2' = match (expr builder e2) with
            (c , A.Primitive,_) -> c
          | (p , _,_)           -> L.const_inttoptr p (L.pointer_type i32_t)
            (*TODO-ADAM: this is a dummy for array math*)
          in
	  ((match op with
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
	  ) e1' e2' "tmp" builder, A.Primitive, 0)
      | A.Unop(op, e) ->
          let e' = match (expr builder e) with
            (c , A.Primitive,_) -> c 
          | (p , _,_) -> L.const_inttoptr p (L.pointer_type i32_t)
            (*TODO-ADAM: lvalue_array_idx does codegen here*)
          in
	  ((match op with
	    A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder, A.Primitive, 0)
      | A.Crement(opDir, op, lv) ->
          (match opDir with
            A.Pre  -> expr builder (A.Assign(lv, (match op with
              A.PlusPlus   -> A.AssnAdd
            | A.MinusMinus -> A.AssnSub), (A.Primary (A.IntLit 1))))
          | A.Post ->
              (* TODO-ADAM: THROWING AWAY VALUE*)
              let (value, decl,_,_) = lvalue builder lv in
              ignore(expr builder (A.Crement(A.Pre, op, lv))); (value, decl, 0)
          )
      | A.Assign (lv, op, e) ->
          let value = (A.Primary (A.Lvalue lv))
          and addr  = (match lv with
            A.Id(s)    -> lookup_addr s
          | A.Arr(s,i) ->
              let s' = lookup_addr s
              and i' = L.const_int i32_t i in
              L.build_in_bounds_gep s' [|i'|] "tmp" builder) in
          let eval = (match op with
            A.AssnReg     -> expr builder e
          | A.AssnAdd     -> expr builder (A.Binop(value, A.Add,  e))
          | A.AssnSub     -> expr builder (A.Binop(value, A.Sub,  e))
          | A.AssnMult    -> expr builder (A.Binop(value, A.Mult, e))
          | A.AssnDiv     -> expr builder (A.Binop(value, A.Div,  e))
          | A.AssnMod     -> expr builder (A.Binop(value, A.Mod,  e))
          ) in
          let eval' = match eval with (p, _,_) -> p in
          (*ignore ((match lv with
            A.Id(s)    -> store_primitive (lookup s) i32_t A.DeclAssnYes [A.Literal(eval)] builder
          | A.Arr(s,i) -> store_array_idx (lookup s) i32_t i A.DeclAssnYes [A.Literal(eval)] builder 
          )); eval*)
          ignore (L.build_store eval' addr builder); eval
      | A.Call ("putchar", [e]) ->
        (L.build_call putchar_func [|(match expr builder e with (p,_,_)->p)|] "putchar" builder, A.Primitive, 0)
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (fun a ->
           match expr builder a with (p,_,_)->p) (List.rev act)) in
	 let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         (* TODO-ADAM: convert fdecl.A.typ to A.decl *)
         (L.build_call fdef (Array.of_list actuals) result builder, A.Primitive, 0)
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None   -> ignore (f builder) in
	
    (* Build llvm code for function statements; return the builder for the statement's successor *)
    (*let dummy_bb = L.append_block context "dummy.toremove.block" the_function in
    let break_builder = dummy_bb and continue_builder = dummy_bb in*)
    let rec stmt (builder, break_bb, cont_bb) = function
      A.Block sl ->
        List.fold_left stmt (builder, break_bb, cont_bb) sl
    | A.Expr e ->
        ignore (expr builder e); (builder, break_bb, cont_bb)
    | A.Break -> 
        ignore(add_terminal builder (L.build_br break_bb));
        let new_block = L.append_block context "after.break" the_function in
        let builder = L.builder_at_end context new_block in (builder, break_bb, cont_bb)
    | A.Continue ->  
        ignore(add_terminal builder (L.build_br cont_bb));
        let new_block = L.append_block context "after.cont" the_function in
        let builder = L.builder_at_end context new_block in (builder, break_bb, cont_bb)
    | A.Return e ->
        ignore (match fdecl.A.typ with
          A.Void -> L.build_ret_void builder
        (*TODO-ADAM: throwing away value*)
        | _      -> L.build_ret (match expr builder e with (p,_,_)->p) builder); (builder, break_bb, cont_bb)
    | A.If (predicate, then_stmt, else_stmt) ->
        (*TODO-ADAM: throwing away value*)
        let bool_val = match expr builder predicate with (p,_,_)->p in
        let if_merge_bb = L.append_block context "if.else.merge" the_function in

        let if_then_bb = L.append_block context "if.then" the_function in
        let b = L.builder_at_end context if_then_bb in
        let (temp1, _, _) = stmt (b, break_bb, cont_bb) then_stmt in 
        ignore(add_terminal temp1 (L.build_br if_merge_bb));

        let if_else_bb = L.append_block context "if.else" the_function in
        let b = L.builder_at_end context if_else_bb in
        let (temp1, _, _) = stmt (b, break_bb, cont_bb) else_stmt in

        ignore(add_terminal temp1 (L.build_br if_merge_bb));
        ignore (L.build_cond_br bool_val if_then_bb if_else_bb builder);
        ((L.builder_at_end context if_merge_bb), break_bb, cont_bb)
    | A.While (predicate, body) ->
        let while_pred_bb = L.append_block context "while.cmp.block" the_function in
        ignore (L.build_br while_pred_bb builder);
        let while_body_bb = L.append_block context "while.body" the_function in
        let while_merge_bb = L.append_block context "while.merge.block" the_function in
        let break_builder = while_merge_bb and continue_builder = while_pred_bb in
        let b = L.builder_at_end context while_body_bb in
        let (temp1, _, _) = stmt (b, break_builder, continue_builder) body in
        ignore(add_terminal temp1 (L.build_br while_pred_bb)); 
        (*if(L.fold_left_instrs ~f:(s->is_terminator s) ~init:() temp1)  (*instr_opcode*)
        then{ 
          ignore(add_terminal temp1 (L.build_br while_pred_bb)); 
        }
        else{
          ignore(add_terminal temp1 (L.build_br while_pred_bb)); 
        }*)
        let pred_builder = L.builder_at_end context while_pred_bb in
        (*TODO-ADAM: throwing away value*)
        let bool_val = match expr pred_builder predicate with (p,_,_)->p in
        ignore (L.build_cond_br bool_val while_body_bb while_merge_bb pred_builder);
        (*ignore(L.replace_all_uses_with (L.build_br dummy_bb) (L.build_br while_merge_bb));*)
        ((L.builder_at_end context while_merge_bb), break_builder, continue_builder)
    | A.For (e1, e2, e3, body) -> 
        stmt (builder, break_bb, cont_bb)
        ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in
    (* Build llvm code for each statement in a function *)
    let dummy_bb = L.append_block context "dummy.toremove.block" the_function in
    let break_builder = dummy_bb and continue_builder = dummy_bb in
    let (builder, _, _) = (stmt (builder, break_builder, continue_builder) (A.Block fdecl.A.body)) 
    in 
    (*let builder = L.builder_at_end context dummy_bb in
    let rec vist_bb_add_terminals = fold_left_blocks (L.block_terminator x) in
    visit_bb_add_terminals the_function*)
    (* Add a return if the last basic block is at the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0));
    ignore(L.builder_at_end context dummy_bb);
    ignore(L.block_terminator dummy_bb);
    ignore(L.delete_block dummy_bb);
  in

  List.iter build_function_body functions;
  the_module
