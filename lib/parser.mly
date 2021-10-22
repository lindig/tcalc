%token <float> NUM
%token PLUS MINUS TIMES DIV CARET
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%right CARET

%start main             /* the entry point */
%type <float> main
%%

main:
    expr EOL                { $1 }
;
expr:
    NUM                     { $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { $1 +. $3 }
  | expr MINUS expr         { $1 -. $3 }
  | expr TIMES expr         { $1 *. $3 }
  | expr DIV expr           { $1 /. $3 }
  | expr CARET expr         { Float.pow $1 $3 }
  | MINUS expr %prec UMINUS { -. $2 }
;
