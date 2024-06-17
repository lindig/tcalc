
%{

let fail fmt = Printf.ksprintf failwith fmt

let t = Hashtbl.create 10

let () = List.iter (fun (id, num) -> Hashtbl.replace t id num)
  [ ("e", Float.exp 1.0)
  ; ("pi", Float.pi)
  ]

let add id num = Hashtbl.replace t id num

let lookup id = Hashtbl.find_opt t id |> function
  | Some v -> v
  | None -> fail "unknown value %s" id

let apply f args = match f, args with
  | "log10", [x] -> Float.log10 x
  | "log", [x] -> Float.log x
  | "logn", [x; y] -> Float.log x /. Float.log y
  | "sin", [x] -> Float.sin x
  | "cos", [x] -> Float.cos x
  | "tan", [x] -> Float.tan x
  | "abs", [x] -> Float.abs x
  | id, [] -> lookup id
  | id, _ -> fail "function %s/%d is unknown" id (List.length args)

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
%type <float option> main
%%

main:
      expr EOL              { add "_" $1; Some $1 }
    | stmt EOL              { None    }
;

stmt:
    ID EQUAL expr           { add $1 $3; $3 }
;

app:
    ID       %prec ID       { ($1, []) }
  | app expr %prec ID       { match $1 with (f, args) -> (f, $2 :: args) } 
  ;

expr:
    NUM                     { $1 }
  | app %prec ID            { match $1 with (f, args) -> apply f (List.rev args)}
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { $1 +. $3 }
  | expr MINUS expr         { $1 -. $3 }
  | expr TIMES expr         { $1 *. $3 }
  | expr DIV expr           { $1 /. $3 }
  | expr CARET expr         { Float.pow $1 $3 }
  | MINUS expr %prec UMINUS { -. $2 }
;
