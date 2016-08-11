(* 
Project:  COMS S4115, SimpliCty Compiler
Filename: src/ast.ml
Authors:  - Rui Gu,           rg2970
          - Adam Hadar,       anh2130
          - Zachary Moffitt,  znm2104
          - Suzanna Schmeelk, ss4648
Purpose:  * Generate abstract syntax tree
          * Functions for printing the AST
Modified: 2016-07-25
*)

type decl = Primitive | Array (* | Struct *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type crement = PlusPlus | MinusMinus

type crementDir = Pre | Post

type typ = Int | Float | Bool | Void | Char

type assn = AssnReg | AssnAdd | AssnSub | AssnMult | AssnDiv | AssnMod

type lvalue = 
    Id of string
  (*| Arr of string * int *)

type primary =
    IntLit of int
  | FloatLit of float 
  | CharLit of char
  | BoolLit of bool
  | Lvalue of lvalue

type expr =
    Primary of primary
  | ArrLit of expr list
  | Lvarr of lvalue * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Crement of crementDir * crement * expr
  | Assign of expr * assn * expr
  | Call of string * expr list
  | Noexpr

type parameter = typ * string * decl * (int list)

type declaration = typ * string * decl * (int list) * (primary list)

type function_declaration = typ * string * decl * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | Break 
  | Continue 
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type extern_func_decl = {
    e_typ : typ;
    e_fname : string;
    e_formals : parameter list;
  }

type func_decl = {
    typ : typ;
    fname : string;
    formals : parameter list;
    locals : declaration list;
    body : stmt list;
  }

type program = declaration list * extern_func_decl list * func_decl list

(* Pretty-printing functions *)

let string_of_decl = function
    Primitive -> "prime"
  | Array -> "array"
  (*| Struct -> "struct"*)

let string_of_op = function
    Add     -> "+"
  | Sub     -> "-"
  | Mult    -> "*"
  | Div     -> "/"
  | Mod     -> "%"
  | Equal   -> "=="
  | Neq     -> "!="
  | Less    -> "<"
  | Leq     -> "<="
  | Greater -> ">"
  | Geq     -> ">="
  | And     -> "&&"
  | Or      -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_crement = function
    PlusPlus   -> "++"
  | MinusMinus -> "--"

let string_of_crementDir = function
    Pre -> "pre"
  | Post-> "post"

let string_of_assn = function
    AssnReg  -> "="
  | AssnAdd  -> "+="
  | AssnSub  -> "-="
  | AssnMult -> "*="
  | AssnDiv  -> "/="
  | AssnMod  -> "%="

let string_of_lvalue = function
    Id(s)    -> s

let string_of_primary = function
    IntLit(i)     -> string_of_int i 
  | FloatLit(f)   -> string_of_float f 
  | CharLit(c)    -> string_of_int (int_of_char c)
  | BoolLit(l)    -> if l = true then "true" else "false"
  | Lvalue(l)     -> string_of_lvalue l

let rec string_of_expr = function
    Primary(l)          ->
      string_of_primary l
  | ArrLit(lp) ->
	"{|"^ String.concat ", " (List.map string_of_expr lp) ^ "|}"
  | Lvarr(lv, e)        ->
      string_of_lvalue lv ^"["^ string_of_expr e ^"]"
  | Binop(e1, o, e2)    ->
      string_of_expr e1 ^" "^ string_of_op o ^" "^ string_of_expr e2
  | Unop(o, e)          ->
      string_of_uop o ^ string_of_expr e
  | Crement(oD, o, e_lv)-> (match oD with
      Pre  -> string_of_crement o ^" "^ string_of_expr e_lv
    | Post -> string_of_expr e_lv ^" "^ string_of_crement o)
  | Assign(e_lv, o, e)  ->
      string_of_expr e_lv ^" "^ string_of_assn o ^" "^ string_of_expr e
  | Call(f, el)         ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^")"
  | Noexpr              -> ""

let rec string_of_stmt = function
    Block(stmts)        ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^"}\n"
  | Expr(expr)          -> string_of_expr expr ^";\n";
  | Break               -> "break;\n";
  | Continue            -> "continue;\n";
  | Return(expr)        -> "return "^ string_of_expr expr ^";\n";
  | If(e, s, Block([])) ->
      "if ("^ string_of_expr e ^")\n"^ string_of_stmt s
  | If(e, s1, s2)       ->
      "if ("^ string_of_expr e ^")\n"^
      string_of_stmt s1 ^
      "else\n"^ string_of_stmt s2
  | For(e1, e2, e3, s)  ->
      "for (" ^ string_of_expr e1 ^" ; "^ string_of_expr e2 ^" ; "^
      string_of_expr e3  ^") "^ string_of_stmt s
  | While(e, s)         -> "while ("^ string_of_expr e ^") "^ string_of_stmt s

let string_of_typ = function
    Int   -> "int"
  | Float -> "float"
  | Char  -> "char"
  | Bool  -> "bool"
  | Void  -> "void"

let string_of_vdecl (t, id, decl, size_list, prim_list) =
   let size' =
     if decl = Primitive then ""
     else "["^ string_of_int (List.hd size_list) ^"]"
   and assn =
     if List.length prim_list = 0 then ""
     else
       let value = 
         if decl = Primitive then string_of_primary (List.hd prim_list)
         else "{|"^ String.concat ", " (List.map string_of_primary prim_list) ^ "|}"
       in
       " = " ^ value
   in
   string_of_typ t ^ size' ^" "^ id ^ assn ^";\n"

let snd_of_four (_,id,_,_) = id 

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd_of_four fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_extern_fdecl efdecl =
  string_of_typ efdecl.e_typ ^ " " ^
  efdecl.e_fname ^ "(" ^ String.concat ", " (List.map snd_of_four efdecl.e_formals) ^
  ");\n"

let string_of_program (vars, externs, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_extern_fdecl externs) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
