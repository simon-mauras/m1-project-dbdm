%{
open Sigs
%}

%token <Sigs.parse_constant> CONSTANT
%token <Sigs.parse_name> NAME
%token SOURCE TARGET MAPPING
%token ARROW COMMA DOT
%token LPAR RPAR
%token DOLLAR
%token EOF

%type <Sigs.parse_main> main

%start main
%%

main:
  | SOURCE schema TARGET schema MAPPING tgds EOF { ($2, $4, $6) }
;
schema:
  | relation schema       { $1 :: $2 }
  | relation              { [$1] }
;
relation:
  | NAME LPAR atts RPAR   { ($1, $3) }
;
atts:
  | NAME COMMA atts       { $1 :: $3 }
  | NAME                  { [$1] }
;
tgds:
  | tgd tgds              { $1 :: $2 }
  | tgd                   { [$1] }
;
tgd:
  | query ARROW query DOT { ($1, $3) }
;
query:
  | atom COMMA query      { $1 :: $3 }
  | atom                  { [$1] }
;
atom:
  | NAME LPAR args RPAR   { ($1, $3) }
;
args:
  | value COMMA args      { $1 :: $3 }
  | value                 { [$1] }  
;
value:
  | CONSTANT              { Cst $1 }
  | DOLLAR NAME           { Var $2 }
;
