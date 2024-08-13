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

let () =
  List.iter
    (fun (id, num) -> Hashtbl.replace t id num)
    [ ("e", Float.exp 1.0); ("pi", Float.pi) ]

let define id num = Hashtbl.replace t id num

let lookup id =
  Hashtbl.find_opt t id |> function
  | Some v -> v
  | None -> fail "unknown value %s" id

let apply f args =
  match (f, args) with
  | "log10", [ x ] -> Float.log10 x
  | "log", [ x ] -> Float.log x
  | "logn", [ x; y ] -> Float.log x /. Float.log y
  | "sin", [ x ] -> Float.sin x
  | "cos", [ x ] -> Float.cos x
  | "tan", [ x ] -> Float.tan x
  | "abs", [ x ] -> Float.abs x
  | "asin", [ x ] -> Float.asin x
  | "acos", [ x ] -> Float.acos x
  | "atan", [ x ] -> Float.atan x
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
