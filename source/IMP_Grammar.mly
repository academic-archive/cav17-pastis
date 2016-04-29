/* Quentin Carbonneaux - 2016 */
%{
  open Types.IMP
%}

%token TFUNC TVAR TTRUE TFALSE TRANDOM TSKIP TWHILE TDO TLOOP
%token TBREAK TRETURNS TEND TIF TTHEN TELSE TNOT TAND TOR TASSUME
%token TSEMI TCOMMA TLPAR TRPAR TLEQ TLT TGEQ TGT TEQ TNE TADD TSUB TMUL
%token TEOF
%token <Types.IMP.id> TIDENT
%token <int> TNUM

%left TOR
%left TAND
%nonassoc TNOT
%nonassoc TEQ TNE TGEQ TGT TLEQ TLT
%left TADD TSUB
%left TMUL

%type <Types.IMP.func list> file
%start file
%%

file: rfuncs TEOF { List.rev $1 }

expr:
  | TIDENT         { EVar $1 }
  | TNUM           { ENum $1 }
  | TSUB expr      { ESub (ENum 0, $2) }
  | expr TADD expr { EAdd ($1, $3) }
  | expr TSUB expr { ESub ($1, $3) }
  | expr TMUL expr { EMul ($1, $3) }
  | TLPAR expr TRPAR { $2 }

exprr:
  | TRANDOM { ERandom }
  | expr    { $1 }

logic:
    TTRUE            { LTrue }
  | TFALSE           { LFalse }
  | TRANDOM          { LRandom }
  | expr TLEQ expr   { LLE ($1, $3) }
  | expr TLT expr    { LLT ($1, $3) }
  | expr TGEQ expr   { LGE ($1, $3) }
  | expr TGT expr    { LGT ($1, $3) }
  | expr TEQ expr    { LEQ ($1, $3) }
  | expr TNE expr    { LNE ($1, $3) }
  | logic TAND logic { LAnd ($1, $3) }
  | logic TOR logic  { LOr ($1, $3) }
  | TNOT logic       { LNot $2 }
  | TLPAR logic TRPAR { $2 }

rexprrs1:
    exprr                 { [$1] }
  | rexprrs1 TCOMMA exprr { $3 :: $1 }

exprrs:
    /* empty */ { [] }
  | rexprrs1    { List.rev $1 }

rids1:
    TIDENT              { [$1] }
  | rids1 TCOMMA TIDENT { $3 :: $1 }

ids:
    /* empty */ { [] }
  | rids1       { List.rev $1 }

instr:
    TBREAK TSEMI                           { IBreak }
  | TASSUME logic TSEMI                    { IAssume $2 }
  | TIDENT TEQ exprr TSEMI                 { IAssign ($1, $3) }
  | TIF logic TTHEN block TEND             { IIf ($2, $4) }
  | TIF logic TTHEN block TELSE block TEND { IIfElse ($2, $4, $6) }
  | TWHILE logic TDO block TEND            { IWhile ($2, $4) }
  | TLOOP block TEND                       { ILoop $2 }
  | TLPAR ids TRPAR TEQ
    TIDENT TLPAR exprrs TRPAR TSEMI        { ICall ($2, $5, $7) }

rinstrs1:
    instr          { [$1] }
  | rinstrs1 instr { $2 :: $1 }

block:
    /* empty */ { [] }
  | rinstrs1    { List.rev $1 }

retopt:
    /* empty */              { [] }
  | TRETURNS TLPAR ids TRPAR { $3 }

varopt:
    /* empty */    { [] }
  | TVAR ids TSEMI { $2 }

func:
  TFUNC TIDENT TLPAR ids TRPAR
  retopt TDO varopt block TEND
  { { fun_name = $2; fun_vars = $8; fun_args = $4
    ; fun_rets = $6; fun_body = $9 }
  }

rfuncs:
    func        { [$1] }
  | rfuncs func { $2 :: $1 }
