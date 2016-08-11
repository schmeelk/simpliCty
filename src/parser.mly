/* 
Project:  COMS S4115, SimpliCty Compiler
Filename: src/parser.mly
Authors:  - Rui Gu,           rg2970
          - Adam Hadar,       anh2130
          - Zachary Moffitt,  znm2104
          - Suzanna Schmeelk, ss4648
Purpose:  * Ocamlyacc parser for SimpliCty
Modified: 2016-07-25
*/

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SINGLEQT OPENARR CLOSEARR
%token PLUS MINUS TIMES DIVIDE MODULO
%token NOT PLUSPLUS MINUSMINUS
%token ASSIGNREG ASSIGNADD ASSIGNSUB ASSIGNMULT ASSIGNDIV ASSIGNMOD
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN BREAK CONTINUE IF ELSE FOR WHILE INT FLOAT BOOL VOID CHAR
%token PRINT EXTERN
%token <int> INTLIT
%token <string> ID
%token <float> FLOATLIT
%token <char> CHARLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc PRINT EXTERN
%right ASSIGNADD ASSIGNSUB ASSIGNMULT ASSIGNDIV ASSIGNMOD
%right ASSIGNREG
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%nonassoc PLUSPLUS MINUSMINUS
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  modul_definitions EOF { $1 }

modul_definitions:
    /* nothing */ { [], [], [] }
 | modul_definitions declaration     { let (a, b, c) = $1 in ($2 :: a), b, c }
 | modul_definitions extern_func_decl { let (a, b, c) = $1 in a, ($2 :: b), c }
 | modul_definitions func_definition { let (a, b, c) = $1 in a, b, ($2 :: c) }

func_definition:
   typ_specifier ID LPAREN parameter_list_opt RPAREN LBRACE declaration_list statement_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

extern_func_decl:
   EXTERN typ_specifier ID LPAREN parameter_list_opt RPAREN SEMI
     { { e_typ = $2;
	 e_fname = $3;
	 e_formals = $5 } }

parameter_list_opt:
    /* nothing */ { [] }
  | parameter_list   { List.rev $1 }

parameter_list:
  | parameter { [$1] }
  | parameter_list COMMA  parameter { $3 :: $1 }
  
parameter:
    typ_specifier ID                          { ($1,$2, Primitive,  0) }
  | typ_specifier LBRACKET RBRACKET ID        { ($1,$4, Array,      0) }
  | typ_specifier arr_size_decl ID { ($1,$3, Array,     $2) }

typ_specifier:
    INT   { Int }
  | FLOAT { Float }
  | CHAR  { Char }
  | BOOL  { Bool }
  | VOID  { Void }

declaration_list:
    /* nothing */    { [] }
  | declaration_list declaration { $2 :: $1 }

declaration:
    typ_specifier ID SEMI                                         { ($1, $2, Primitive, 0,  []) }
  | typ_specifier ID ASSIGNREG primary SEMI                       { ($1, $2, Primitive, 0, [$4]) }
  | typ_specifier arr_size_decl ID SEMI                           { ($1, $3, Array,    $2,  []) }
  | typ_specifier arr_size_decl ID ASSIGNREG decl_assign_arr SEMI { ($1, $3, Array,    $2, $5) }

decl_assign_arr:
    OPENARR arr_assign CLOSEARR {List.rev $2}

arr_assign:
    primary {[$1]}
  | arr_assign COMMA primary {$3::$1}

arr_size_decl:
    LBRACKET INTLIT RBRACKET {$2}

statement_list:
    /* nothing */  { [] }
  | statement_list statement { $2 :: $1 }

statement:
    expression SEMI              { Expr $1 }
  | LBRACE statement_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expression RPAREN statement %prec NOELSE   { If($3, $5, Block([])) }
  | IF LPAREN expression RPAREN statement ELSE statement { If($3, $5, $7) }
  | WHILE LPAREN expression RPAREN statement { While($3, $5) }
  | FOR LPAREN expression_opt SEMI expression SEMI expression_opt RPAREN statement
     { For($3, $5, $7, $9) }
  | BREAK SEMI             { Break }
  | CONTINUE SEMI          { Continue }
  | RETURN SEMI            { Return Noexpr }
  | RETURN expression SEMI { Return $2 }

expression_opt:
    /* nothing */ { Noexpr }
  | expression    { $1 }

expression:
    primary                      { Primary($1) }
  | LPAREN expression RPAREN     { $2 }
  | OPENARR expression_list CLOSEARR {ArrLit($2)}
  | lvalue LBRACKET expression RBRACKET { Lvarr($1,$3) }
  | expression PLUS   expression { Binop($1, Add,     $3) }
  | expression MINUS  expression { Binop($1, Sub,     $3) }
  | expression TIMES  expression { Binop($1, Mult,    $3) }
  | expression DIVIDE expression { Binop($1, Div,     $3) }
  | expression MODULO expression { Binop($1, Mod,     $3) }
  | expression EQ     expression { Binop($1, Equal,   $3) }
  | expression NEQ    expression { Binop($1, Neq,     $3) }
  | expression LT     expression { Binop($1, Less,    $3) }
  | expression LEQ    expression { Binop($1, Leq,     $3) }
  | expression GT     expression { Binop($1, Greater, $3) }
  | expression GEQ    expression { Binop($1, Geq,     $3) }
  | expression AND    expression { Binop($1, And,     $3) }
  | expression OR     expression { Binop($1, Or,      $3) }
  | MINUS expression %prec NEG   { Unop(Neg, $2) }
  | NOT expression               { Unop(Not, $2) }
  | PLUSPLUS expression          { Crement(Pre,  PlusPlus,   $2) }
  | MINUSMINUS expression        { Crement(Pre,  MinusMinus, $2) }
  | expression PLUSPLUS          { Crement(Post, PlusPlus,   $1) }
  | expression MINUSMINUS        { Crement(Post, MinusMinus, $1) }
  | expression ASSIGNREG expression  { Assign($1, AssnReg,  $3) }
  | expression ASSIGNADD expression  { Assign($1, AssnAdd,  $3) }
  | expression ASSIGNSUB expression  { Assign($1, AssnSub,  $3) }
  | expression ASSIGNMULT expression { Assign($1, AssnMult, $3) }
  | expression ASSIGNDIV expression  { Assign($1, AssnDiv,  $3) }
  | expression ASSIGNMOD expression  { Assign($1, AssnMod,  $3) }
  | ID LPAREN expression_list_opt RPAREN { Call($1, $3) }

primary:
    INTLIT   { IntLit($1) }
  | FLOATLIT { FloatLit($1) }
  | CHARLIT  { CharLit($1) }
  | TRUE     { BoolLit(true) }
  | FALSE    { BoolLit(false) }
  | lvalue   { Lvalue($1) }

lvalue:
    ID                              { Id($1) }
  /*| ID LBRACKET expression RBRACKET { Arr($1,$3) }*/

expression_list_opt:
    /* nothing */ { [] }
  | expression_list  { List.rev $1 }

expression_list:
    expression                       { [$1] }
  | expression_list COMMA expression { $3 :: $1 }
