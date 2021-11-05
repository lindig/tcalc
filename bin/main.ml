(*
 * main
 *)

let rec repl prompt cb =
  match LNoise.linenoise prompt with
  | None       -> ()
  | Some ""    -> repl prompt cb
  | Some input ->
      cb input;
      ignore @@ LNoise.history_add input;
      repl prompt cb

let ( // ) x y =
  let div = x /. y |> Float.trunc in
  let rem = x -. (div *. y) in
  (div, rem)

let hms seconds =
  let h, s = seconds // 3600.0 in
  let m, s = s // 60.0 in
  (h, m, s)

let result seconds =
  let h, m, s = hms seconds in
  Printf.printf "%5.2f (%02.0f:%02.0f:%05.2f)\n%!" seconds h m s

let main () =
  ignore @@ LNoise.history_set ~max_length:100;
  let calc input =
    try
      let lexbuf = Lexing.from_string input in
      let seconds = Tcalc.Parser.main Tcalc.Lexer.token lexbuf in
      result seconds
    with _ -> Printf.printf "syntax error in \"%s\"\n%!" input
  in
  repl "tcalc> " calc

let () = if !Sys.interactive then () else main ()
