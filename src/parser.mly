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

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA SINGLEQT
%token PLUS MINUS TIMES DIVIDE MODULO
%token NOT PLUSPLUS MINUSMINUS
%token ASSIGNREG ASSIGNADD ASSIGNSUB ASSIGNMULT ASSIGNDIV ASSIGNMOD
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID CHAR
%token PRINT
%token <int> LITERAL
%token <string> ID
%token <char> CHARLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc PRINT
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
   /* nothing */ { [], [] }
 | modul_definitions declaration     { ($2 :: fst $1), snd $1 }
 | modul_definitions func_definition { fst $1, ($2 :: snd $1) }

func_definition:
   typ_specifier ID LPAREN parameter_list_opt RPAREN LBRACE declaration_list statement_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

parameter_list_opt:
    /* nothing */ { [] }
  | parameter_list   { List.rev $1 }

parameter_list:
  | parameter { [$1] }
  | parameter_list COMMA  parameter { $3 :: $1 }
  
parameter:
    typ_specifier ID { ($1,$2, Primitive, Primary(Literal(0))) }
  | typ_specifier LBRACKET expression RBRACKET ID    { ($1, $5, Array, $3) }

typ_specifier:
    INT  { Int }
  | CHAR { Char }
  | BOOL { Bool }
  | VOID { Void }

declaration_list:
    /* nothing */    { [] }
  | declaration_list declaration { $2 :: $1 }

declaration:
    typ_specifier ID SEMI                              { ($1, $2, Primitive, Primary(Literal(0))) }
  | typ_specifier LBRACKET expression RBRACKET ID SEMI { ($1, $5, Array, $3) }

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
  | RETURN SEMI            { Return Noexpr }
  | RETURN expression SEMI { Return $2 }

expression_opt:
    /* nothing */ { Noexpr }
  | expression    { $1 }

expression:
    primary                      { Primary($1) }
  | LPAREN expression RPAREN     { $2 }
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
  | PLUSPLUS lvalue              { Crement(Pre,  PlusPlus,   $2) }
  | MINUSMINUS lvalue            { Crement(Pre,  MinusMinus, $2) }
  | lvalue PLUSPLUS              { Crement(Post, PlusPlus,   $1) }
  | lvalue MINUSMINUS            { Crement(Post, MinusMinus, $1) }
  | lvalue ASSIGNREG expression  { Assign($1, AssnReg,  $3) }
  | lvalue ASSIGNREG SINGLEQT expression SINGLEQT  { Assign($1, AssnReg, $4) }
  | lvalue ASSIGNADD expression  { Assign($1, AssnAdd,  $3) }
  | lvalue ASSIGNSUB expression  { Assign($1, AssnSub,  $3) }
  | lvalue ASSIGNMULT expression { Assign($1, AssnMult, $3) }
  | lvalue ASSIGNDIV expression  { Assign($1, AssnDiv,  $3) }
  | lvalue ASSIGNMOD  expression  { Assign($1, AssnMod,  $3) }
  | ID LPAREN expression_list_opt RPAREN { Call($1, $3) }

primary:
    LITERAL { Literal($1) }
  | TRUE    { BoolLit(true) }
  | FALSE   { BoolLit(false) }
  | lvalue  { Lvalue($1) }

lvalue:
    ID                           { Id($1) }
  | ID LBRACKET LITERAL RBRACKET { Arr($1,$3) }


expression_list_opt:
    /* nothing */ { [] }
  | expression_list  { List.rev $1 }

expression_list:
    expression                       { [$1] }
  | expression_list COMMA expression { $3 :: $1 }
