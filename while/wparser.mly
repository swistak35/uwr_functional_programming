%{
  open Printf
  open Lexing
  open Wast

  let info_error n = 
    let start_pos = Parsing.rhs_start_pos n in
    let end_pos = Parsing.rhs_end_pos n in
        printf "%d.%d-%d.%d: Syntax error\n"
          start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
          end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)
%}

/* Ocamlyacc Declarations */
%token LPAREN RPAREN
%token IF THEN ELSE
%token WHILE DO DONE
%token BEGIN END PROC IS VAR
%token SKIP ASSGN CALL
%token CONS
%token EQ LEQ
%token TRUE FALSE NOT AND
%token EOF
%token WRITE READ
%token TRY RESCUE WITH RAISE

%token <int> INUM
%token PLUS MINUS MULT
%token <string> ID

%left PLUS MINUS
%left MULT
%left AND
%left NOT
%left NEG
%left CONS
%nonassoc EQ LEQ


%start input
%type <Wast.ws> input

/* Grammar follows */
%%

/* Main input */
input:    stms EOF                            { $1 }
;

/* List of statements */
stms:     /* empty */                         { Skip }
          | cstm stms                          { Cons($1, $2) }
;

/* Arithmetic expressions */
aexp:     | INUM                              { Constant $1 }
          | ID                                { Var($1) }
          | aexp PLUS aexp                    { Add($1, $3) }
          | aexp MINUS aexp                   { Sub($1, $3) }
          | aexp MULT aexp                    { Mul($1, $3) }
          | MINUS aexp %prec NEG              { Neg($2) }
          | LPAREN aexp RPAREN                { $2 }
          /*
            Tutaj nie robimy error recovery, bo aexp moze być częścią bexp lub stm,
            ale nie na odwrót, więc wystarczy jeśli damy error recovery tam.
            Pojawiałby się tutaj wtedy reduce/reduce conflict.
          */
;

/* Boolean expressions */
bexp:     | TRUE                              { TrueValue }
          | FALSE                             { FalseValue }
          | aexp EQ aexp                      { Equal($1, $3) }
          | aexp LEQ aexp                     { Leq($1, $3) }
          | NOT bexp                          { Not($2) }
          | bexp AND bexp                     { And($1, $3) }
          | LPAREN bexp RPAREN                { $2 }
          | LPAREN error RPAREN               { TrueValue } /* sth of type AST ws needs to be here */
;

/* Statements, which are considered full (doesn't need CONS) */
cstm:     | stm CONS                          { $1 }
          | IF bexp THEN cstm ELSE cstm       { Cond($2, $4, $6) }
          | WHILE bexp DO cstm                { While($2, $4) }
          | TRY cstm catch                    { Try($2, $3) }
          | BEGIN var_dec proc_dec stms END   { Block($2, $3, $4) }
          | error CONS                        { info_error 1; Skip } /* sth of type AST ws needs to be here */
;

/* Statements */
stm:      /* empty */                         { Skip } /* sth of type AST ws needs to be here */
          | SKIP                              { Skip }
          | ID ASSGN aexp                     { Assgn($1, $3) }
          | CALL ID LPAREN carg RPAREN        { Call($2, $4) }
          | WRITE ID                          { Write($2) }
          | READ ID                           { Read($2) }
          | LPAREN stms RPAREN                { $2 }
          | RAISE ID                          { Raise($2) }
;

/* Rescue expressions */
catch:    /* empty */                         { RescueNone }
          | RESCUE ID WITH cstm catch         { Rescue($2, $4, $5) }
;

/* Var declarations in blocks */
var_dec:  /* empty*/                          { DecVarNone }
          | VAR ID ASSGN aexp CONS var_dec    { DecVar($2, $4, $6) }
;

/* Proc declarations in blocks */
proc_dec: /* empty */                         { DecProcNone }
          | PROC ID LPAREN arg RPAREN IS cstm proc_dec  { DecProc($2, $4, $7, $8) }
;

/* Arguments in proc definition */
arg:      /* empty */                         { None }
          | ID                                { Some $1 }
;

/* Arguments in proc call */
carg:     /* empty */                         { None }
          | aexp                              { Some $1 }
;

%%