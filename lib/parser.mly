
%{

let fail fmt = Printf.ksprintf failwith fmt

let t = Hashtbl.create 10

let add id num = Hashtbl.replace t id num
let lookup id = Hashtbl.find_opt t id |> function
  | Some v -> v
  | None -> fail "unknown value %s" id

%}

%token <float> NUM
%token <string> ID
%token PLUS MINUS TIMES DIV CARET
%token LPAREN RPAREN
%token EQUAL EOL
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
  | ID                      { lookup $1 }
  | ID EQUAL expr           { add $1 $3; $3 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { $1 +. $3 }
  | expr MINUS expr         { $1 -. $3 }
  | expr TIMES expr         { $1 *. $3 }
  | expr DIV expr           { $1 /. $3 }
  | expr CARET expr         { Float.pow $1 $3 }
  | MINUS expr %prec UMINUS { -. $2 }
;
