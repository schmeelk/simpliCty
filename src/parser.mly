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

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE MODULO
%token NOT PLUSPLUS MINUSMINUS
%token ASSIGNREG ASSIGNADD ASSIGNSUB ASSIGNMULT ASSIGNDIV ASSIGNMOD
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID
%token PRINT
%token <int> LITERAL
%token <string> ID
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
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | typ LBRACKET RBRACKET ID { [($1, $4)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI                         { ($1, $2) }
  | typ LBRACKET expr RBRACKET ID SEMI { ($1, $5) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | PRINT expr SEMI { Print($2) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL                   { Literal($1) }
  | TRUE                      { BoolLit(true) }
  | FALSE                     { BoolLit(false) }
  | ID                        { Id(Primitive, $1, Literal(0)) }
  | ID LBRACKET expr RBRACKET { Id(Array, $1, $3) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MODULO expr { Binop($1, Mod,	$3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr             { Unop(Not, $2) }
  | PLUSPLUS ID          { Crement(Pre,  PlusPlus,   Primitive, $2, Literal(0)) }
  | MINUSMINUS ID        { Crement(Pre,  MinusMinus, Primitive, $2, Literal(0)) }
  | ID PLUSPLUS          { Crement(Post, PlusPlus,   Primitive, $1, Literal(0)) }
  | ID MINUSMINUS        { Crement(Post, MinusMinus, Primitive, $1, Literal(0)) }
  | PLUSPLUS ID LBRACKET expr RBRACKET   {Crement(Pre,  PlusPlus,   Array, $2, $4)}
  | MINUSMINUS ID LBRACKET expr RBRACKET {Crement(Pre,  MinusMinus, Array, $2, $4)}
  | ID LBRACKET expr RBRACKET PLUSPLUS   {Crement(Post, PlusPlus,   Array, $1, $3)}
  | ID LBRACKET expr RBRACKET MINUSMINUS {Crement(Post, MinusMinus, Array, $1, $3)}
  | ID ASSIGNREG expr   { Assign(Primitive,$1,Literal(0), AssnReg, $3) }
  | ID ASSIGNADD expr   { Assign(Primitive,$1,Literal(0), AssnAdd, $3) }
  | ID ASSIGNSUB expr   { Assign(Primitive,$1,Literal(0), AssnSub, $3) }
  | ID ASSIGNMULT expr  { Assign(Primitive,$1,Literal(0), AssnMult, $3) }
  | ID ASSIGNDIV expr   { Assign(Primitive,$1,Literal(0), AssnDiv, $3) }
  | ID ASSIGNMOD expr   { Assign(Primitive,$1,Literal(0), AssnMod, $3) }
  | ID LBRACKET expr RBRACKET ASSIGNREG expr  { Assign(Array,$1,$3, AssnReg, $6) }
  | ID LBRACKET expr RBRACKET ASSIGNADD expr  { Assign(Array,$1,$3, AssnAdd, $6) }
  | ID LBRACKET expr RBRACKET ASSIGNSUB expr  { Assign(Array,$1,$3, AssnSub, $6) }
  | ID LBRACKET expr RBRACKET ASSIGNMULT expr { Assign(Array,$1,$3, AssnMult,$6) }
  | ID LBRACKET expr RBRACKET ASSIGNDIV expr  { Assign(Array,$1,$3, AssnDiv, $6) }
  | ID LBRACKET expr RBRACKET ASSIGNMOD expr  { Assign(Array,$1,$3, AssnMod, $6) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
