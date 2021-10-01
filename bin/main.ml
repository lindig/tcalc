let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Tcalc.Parser.main Tcalc.Lexer.token lexbuf in
      Printf.printf "%5.2f\n" result;
      flush stdout
    done
  with Tcalc.Lexer.EOF -> exit 0

let () = if !Sys.interactive then () else main ()
