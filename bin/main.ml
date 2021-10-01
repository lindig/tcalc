let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Tcalc.Parser.main Tcalc.Lexer.token lexbuf in
      print_int result;
      print_newline ();
      flush stdout
    done
  with Tcalc.Lexer.Eof -> exit 0

let () = if !Sys.interactive then () else main ()
