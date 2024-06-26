
%{

%}

%token <float> NUM
%token <string> ID
%token PLUS MINUS TIMES DIV CARET
%token LPAREN RPAREN
%token EQUAL EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%right CARET
%right ID
%nonassoc LPAREN NUM UMINUS

%start main             /* the entry point */
%type <Ast.t> main
%%

main:
      expr EOL              { Ast.Expr $1 }
    | stmt EOL              { match $1 with
                              id,expr -> Ast.Define (id, expr)
                            }
;

stmt:
    ID EQUAL expr           { ($1, $3) }
;

app:
    ID       %prec ID       { ($1, []) }
  | app expr %prec ID       { match $1 with (f, args) -> (f, $2 :: args) } 
  ;

expr:
    NUM                     { Ast.Const $1 }
  | app %prec ID            { match $1 with (f, args) -> Ast.Apply(f, List.rev args)}
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Ast.(BinOp($1,Add,$3)) }
  | expr MINUS expr         { Ast.(BinOp($1,Sub,$3)) } 
  | expr TIMES expr         { Ast.(BinOp($1,Mul,$3)) }
  | expr DIV expr           { Ast.(BinOp($1,Div,$3)) }
  | expr CARET expr         { Ast.(BinOp($1,Pow,$3)) }
  | MINUS expr %prec UMINUS { Ast.(UnOp(Neg,$2)) }
;
