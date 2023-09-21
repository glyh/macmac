%{
open Ast

%}

%token <bool> BOOL
%token <float> FLOAT
%token <string> KEYWORD
%token <string> STRING
%token <string> IDENT
%token NIL

%token LPAREN
%token RPAREN

%token EOF

%type <value list> program

%start program

%%

program: 
  | fs=list(form) EOF { fs }

form: 
  | b=BOOL { Bool b }
  | f=FLOAT { Float f }
  | kw=KEYWORD { Keyword kw }
  | s=STRING { Str s }
  | sym=IDENT { Sym sym }
  | NIL { Nil }
  | LPAREN RPAREN { Nil }
  | LPAREN f=form fs=list(form) RPAREN { List(f, fs) }
