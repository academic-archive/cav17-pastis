/* Quentin Carbonneaux - 2016 */
%{
  open Types
  open IMP_Types

  let mkb (startp, b, endp) =
    { b_start_p = startp
    ; b_end_p = endp
    ; b_body = b
    }
  let b b_start_p blk = { blk with b_start_p }
  let belse p b = mkb (b.b_end_p, [], b.b_end_p)

%}

%token TVAR TTRUE TFALSE TRANDOM TSKIP TDO TRETURNS
%token TTHEN TNOT TAND TOR TFOCUS TSEMI TCOMMA
%token TRPAR TLEQ TLT TGEQ TGT TEQ TNE TADD TSUB TMUL
%token TEOF TNL
%token <Types.position> TBREAK TASSUME TIF TELSE TWHILE TLOOP TLPAR
%token <Types.position> TDEF TEND TTHEN TDO TWEAK TIN TDE TCOLON TPASS
%token <(Types.id * Types.position)> TIDENT
%token <int> TNUM

%left TOR
%left TAND
%nonassoc TNOT
%nonassoc TEQ TNE TGEQ TGT TLEQ TLT
%left TADD TSUB
%left TMUL

%type <Types.id list * (Types.free_expr, IMP_Types.block) Types.func_ list> file
%start file
%%

file: varopt rfuncs TEOF { $1, List.rev $2 }

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
  | expr TLEQ expr   { LCmp ($1, Le, $3) }
  | expr TLT expr    { LCmp ($1, Lt, $3) }
  | expr TGEQ expr   { LCmp ($1, Ge, $3) }
  | expr TGT expr    { LCmp ($1, Gt, $3) }
  | expr TEQ expr    { LCmp ($1, Eq, $3) }
  | expr TNE expr    { LCmp ($1, Ne, $3) }
  | logic TAND logic { LAnd ($1, $3) }
  | logic TOR logic  { LOr ($1, $3) }
  | TNOT logic       { LNot $2 }
  | TLPAR logic TRPAR { $2 }

rids1:
    ident              { [$1] }
  | rids1 TCOMMA ident { $3 :: $1 }

ids:
    /* empty */ { [] }
  | rids1       { List.rev $1 }

simpl:
  | TBREAK TNL             { IBreak, $1 }
  | TWEAK TNL              { IWeaken, $1 }
  | TASSUME logic TNL      { IAssume $2, $1 }
  | TIDENT TEQ exprr TNL   { IAssign (fst $1, $3), snd $1 }
  | TIDENT TLPAR TRPAR TNL { ICall (fst $1), snd $1 }

instr:
  | simpl                     { $1 }
  | TWHILE logic TCOLON block { IWhile ($2, b $1 $4), $1 }
  | TLOOP TCOLON block        { ILoop (b $2 $3), $1 }
  | TIF logic TCOLON block    { IIf ($2, b $3 $4, belse $3 $4), $1 }
  | TIF logic TCOLON block
    TELSE TCOLON block        { IIf ($2, b $3 $4, b $6 $7), $1 }


rinstrs1:
    instr          { [$1] }
  | rinstrs1 instr { $2 :: $1 }

block:
  | TPASS TNL        { mkb ($1, [], $1) }
  | simpl            { mkb (snd $1, [$1], snd $1) }
  | TIN rinstrs1 TDE { mkb ($1, List.rev $2, $3) }

rfrees:
    free               { [$1] }
  | rfrees TCOMMA free { $3 :: $1 }

frees:
    /* empty */ { [] }
  | rfrees      { List.rev $1 }

free:
    expr                     { FBase $1 }
  | TIDENT TLPAR frees TRPAR { FApply (fst $1, $3, snd $1) }
  | expr TGEQ expr           { FApply (">=", [FBase $1; FBase $3]
                                      , Utils.dummy_position) }

varopt:
    /* empty */  { [] }
  | TVAR ids TNL { $2 }

focusopt:
    /* empty */      { [] }
  | TFOCUS frees TNL { $2 }

func:
  TDEF ident TLPAR TRPAR TCOLON
  TIN varopt focusopt rinstrs1 TDE
  { { fun_name = $2; fun_vars = $7
    ; fun_focus = $8; fun_body = mkb ($5, List.rev $9, $10)
    ; fun_start_p = $1; fun_end_p = $10 }
  }

rfuncs:
    func        { [$1] }
  | rfuncs func { $2 :: $1 }

/*
rexprrs1:
    exprr                 { [$1] }
  | rexprrs1 TCOMMA exprr { $3 :: $1 }

exprrs:
    / * empty * / { [] }
  | rexprrs1    { List.rev $1 }

  | TLPAR ids TRPAR TEQ
    ident TLPAR exprrs TRPAR TSEMI { ICall ($2, $5, $7), $1 }
*/
