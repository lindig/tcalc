type binop = Add | Sub | Mul | Div | Pow
type unop = Neg

type expr =
  | Const of float
  | Id of string
  | BinOp of expr * binop * expr
  | UnOp of unop * expr
  | Apply of string * expr list

type t = Expr of expr | Define of string * expr

let fail fmt = Printf.ksprintf failwith fmt
let t = Hashtbl.create 10

let constants =
  [
    ("e", Float.exp 1.0)
  ; ("pi", Float.pi)
  ; ("length8", 8.4)
  ; ("m8p", (5.0 *. 60.0) +. 18.0)
  ; ("m4x", (5.0 *. 60.0) +. 31.0)
  ; ("m4m", (5.0 *. 60.0) +. 41.0)
  ; ("m4p", (5.0 *. 60.0) +. 53.0)
  ; ("m2x", (6.0 *. 60.0) +. 00.0)
  ; ("m2m", (6.0 *. 60.0) +. 11.0)
  ; ("m1x", (6.0 *. 60.0) +. 32.0)
  ; ("lm8p", (5.0 *. 60.0) +. 26.0)
  ; ("lm4x", (5.0 *. 60.0) +. 39.0)
  ; ("lm4m", (5.0 *. 60.0) +. 46.0)
  ; ("lm4p", (6.0 *. 60.0) +. 01.0)
  ; ("lm2x", (6.0 *. 60.0) +. 07.0)
  ; ("lm2m", (6.0 *. 60.0) +. 20.0)
  ; ("lm1x", (6.0 *. 60.0) +. 38.0)
  ; ("w8p", (5.0 *. 60.0) +. 53.0)
  ; ("w4x", (6.0 *. 60.0) +. 06.0)
  ; ("w4m", (6.0 *. 60.0) +. 18.0)
  ; ("w4p", (6.0 *. 60.0) +. 32.0)
  ; ("w2x", (6.0 *. 60.0) +. 38.0)
  ; ("w2m", (6.0 *. 60.0) +. 51.0)
  ; ("w1x", (7.0 *. 60.0) +. 09.0)
  ; ("lw8p", (6.0 *. 60.0) +. 07.0)
  ; ("lw4x", (6.0 *. 60.0) +. 15.0)
  ; ("lw4m", (6.0 *. 60.0) +. 27.0)
  ; ("lw4p", (6.0 *. 60.0) +. 39.0)
  ; ("lw2x", (6.0 *. 60.0) +. 45.0)
  ; ("lw2m", (7.0 *. 60.0) +. 00.0)
  ; ("lw1x", (7.0 *. 60.0) +. 15.0)
  ]

let () = List.iter (fun (id, num) -> Hashtbl.replace t id num) constants

let symbols =
  [
    "log10"
  ; "log"
  ; "logn"
  ; "cover"
  ; "sin"
  ; "cos"
  ; "tan"
  ; "abs"
  ; "asin"
  ; "acos"
  ; "atan"
  ; "exp"
  ]
  @ List.map fst constants

let is_alpha = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | '0' .. '9' -> true
  | '_' -> true
  | _ -> false

(** split a line into two by splittin off the last word. The last word could be
    used to select a possible completion. If there is no last word (for example,
    the last character is a space), the last word in an empty string *)
let split str =
  let len = String.length str in
  let rec loop = function
    | i when i < 0 -> len
    | i when is_alpha str.[i] -> loop (i - 1)
    | i -> len - i - 1
  in
  let word = loop (len - 1) in
  (String.sub str 0 (len - word), String.sub str (len - word) word)

(** Suggest completions for [line]. This is based on the last word in the line,
    assuming it is a prefix of a word that we could complete. We return a list
    of (extended) lines and not just the completion we found *)
let completions line =
  let left, right = split line in
  List.filter (String.starts_with ~prefix:right) symbols
  |> List.map (fun completion -> String.concat "" [ left; completion ])

let define id num = Hashtbl.replace t id num

let lookup id =
  Hashtbl.find_opt t id |> function
  | Some v -> v
  | None -> fail "unknown value %s" id

let cover dist rate = 30_000.0 /. (rate *. (lookup "length8" +. dist))

let apply f args =
  match (f, args) with
  | "log10", [ x ] -> Float.log10 x
  | "log", [ x ] -> Float.log x
  | "logn", [ x; y ] -> Float.log x /. Float.log y
  | "cover", [ x; y ] -> cover x y
  | "sin", [ x ] -> Float.sin x
  | "cos", [ x ] -> Float.cos x
  | "tan", [ x ] -> Float.tan x
  | "abs", [ x ] -> Float.abs x
  | "asin", [ x ] -> Float.asin x
  | "acos", [ x ] -> Float.acos x
  | "atan", [ x ] -> Float.atan x
  | "exp", [ x ] -> Float.exp x
  | id, [] -> lookup id
  | id, _ -> fail "function %s/%d is unknown" id (List.length args)

let rec eval = function
  | Expr e ->
      let r = eval' e in
      define "_" r;
      Some r
  | Define (id, e) ->
      define id (eval' e);
      None

and eval' = function
  | Const x -> x
  | Id id -> apply id []
  | BinOp (x, Add, y) -> eval' x +. eval' y
  | BinOp (x, Sub, y) -> eval' x -. eval' y
  | BinOp (x, Div, y) -> eval' x /. eval' y
  | BinOp (x, Mul, y) -> eval' x *. eval' y
  | BinOp (x, Pow, y) -> Float.pow (eval' x) (eval' y)
  | UnOp (Neg, x) -> -.eval' x
  | Apply (id, args) -> apply id (List.map eval' args)

let atom x = Sexpr.Atom x
let node head body = Sexpr.Node (head, body)
let sprintf = Printf.sprintf

let rec sexpr = function
  | Expr e -> sexpr' e
  | Define (id, e) -> node "define" [ atom id; sexpr' e ]

and sexpr' = function
  | Const x -> atom (sprintf "%5.2f" x)
  | Id id -> atom id
  | BinOp (x, Add, y) -> node "+" [ sexpr' x; sexpr' y ]
  | BinOp (x, Sub, y) -> node "-" [ sexpr' x; sexpr' y ]
  | BinOp (x, Div, y) -> node "/" [ sexpr' x; sexpr' y ]
  | BinOp (x, Mul, y) -> node "*" [ sexpr' x; sexpr' y ]
  | BinOp (x, Pow, y) -> node "^" [ sexpr' x; sexpr' y ]
  | UnOp (Neg, x) -> node "~" [ sexpr' x ]
  | Apply (id, args) -> node id (List.map sexpr' args)
