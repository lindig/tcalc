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
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let seconds = Tcalc.Parser.main Tcalc.Lexer.token lexbuf in
      result seconds
    done
  with Tcalc.Lexer.EOF -> exit 0

let () = if !Sys.interactive then () else main ()
