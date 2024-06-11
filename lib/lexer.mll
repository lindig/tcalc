{
  open Parser        (* The type token is defined in parser.mli *)
  exception EOF

  let atof = float_of_string
  let hms h m s = h *. 3600.0 +. m *. 60.0 +. s
}

let digit  = ['0'-'9']
let digits = digit digit*
let alpha  = ['a'-'z']
let id     = alpha (digit|alpha)*

let seconds = digits  ('.' digits)?

rule token = parse
  [' ' '\t']      { token lexbuf }     (* skip blanks *)
| ['\n' ]         { EOL }

| seconds  as s   { NUM(hms 0.0 0.0 (atof s)) }

| (digits  as m) ':'
  (seconds as s)  { NUM(hms 0.0 (atof m) (atof s)) }

| (digits  as h) ':'
  (digits  as m) ':'
  (seconds as s)  { NUM(hms (atof h) (atof m) (atof s)) }

| '+'             { PLUS }
| '-'             { MINUS }
| '*'             { TIMES }
| '/'             { DIV }
| '('             { LPAREN }
| ')'             { RPAREN }
| '^'             { CARET }
| '='             { EQUAL }
| id as id        { ID(id) }
| _               { EOL }

| eof             { EOL }
