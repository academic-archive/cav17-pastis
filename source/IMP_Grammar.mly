/* Quentin Carbonneaux - 2016 */
%{
  open Types
  open IMP_Types

  let b startp b endp =
    { b_start_p = startp
    ; b_end_p = endp
    ; b_body = b
    }
%}

%token TVAR TTRUE TFALSE TRANDOM TSKIP TDO TRETURNS
%token TTHEN TNOT TAND TOR TSEMI TCOMMA
%token TRPAR TLEQ TLT TGEQ TGT TEQ TNE TADD TSUB TMUL
%token TEOF
%token <Types.position> TBREAK TASSUME TIF TELSE TWHILE TLOOP TLPAR TFUNC TEND TTHEN TDO TWEAK
%token <(Types.id * Types.position)> TIDENT
%token <int> TNUM

%left TOR
%left TAND
%nonassoc TNOT
%nonassoc TEQ TNE TGEQ TGT TLEQ TLT
%left TADD TSUB
%left TMUL

%type <IMP_Types.func list> file
%start file
%%

file: rfuncs TEOF { List.rev $1 }

ident: TIDENT { fst $1 }

expr:
  | ident          { EVar $1 }
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
    ident              { [$1] }
  | rids1 TCOMMA ident { $3 :: $1 }

ids:
    /* empty */ { [] }
  | rids1       { List.rev $1 }

instr:
  | TBREAK TSEMI                           { IBreak, $1 }
  | TWEAK TSEMI                            { IWeaken, $1 }
  | TASSUME logic TSEMI                    { IAssume $2, $1 }
  | TIDENT TEQ exprr TSEMI                 { IAssign (fst $1, $3), snd $1 }
  | TIF logic TTHEN block TEND             { IIf ($2, b $3 $4 $5, b $5 [] $5), $1 }
  | TIF logic TTHEN block TELSE block TEND { IIf ($2, b $3 $4 $5, b $5 $6 $7), $1 }
  | TWHILE logic TDO block TEND            { IWhile ($2, b $3 $4 $5), $1 }
  | TLOOP block TEND                       { ILoop (b $1 $2 $3), $1 }
  | TLPAR ids TRPAR TEQ
    ident TLPAR exprrs TRPAR TSEMI         { ICall ($2, $5, $7), $1 }

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
  TFUNC ident TLPAR ids TRPAR
  retopt TDO varopt block TEND
  { { fun_name = $2; fun_vars = $8; fun_args = $4
    ; fun_rets = $6; fun_body = b $1 $9 $10
    ; fun_start_p = $1; fun_end_p = $10 }
  }

rfuncs:
    func        { [$1] }
  | rfuncs func { $2 :: $1 }
