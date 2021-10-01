(* File lexer.mll *)
{
  open Parser        (* The type token is defined in parser.mli *)
  exception EOF

}

let digit = ['0'-'9']
let num = 
    digit+ ('.' digit+)*
  | digit*  '.' digit+

rule token = parse
  [' ' '\t']     { token lexbuf }     (* skip blanks *)
| ['\n' ]        { EOL }
| num as lxm     { NUM(float_of_string lxm) }
| '+'            { PLUS }
| '-'            { MINUS }
| '*'            { TIMES }
| '/'            { DIV }
| '('            { LPAREN }
| ')'            { RPAREN }
| eof            { raise EOF }
