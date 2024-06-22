
type binop = Add | Sub | Mul | Div | Pow
type unop = Neg

type expr =
	| Const of float
	| Id of string
	| BinOp of expr * binop * expr
	| UnOp of unop * expr
	| Apply of string * expr list

type t =
	| Expr of expr
	| Define of string * expr

let fail fmt = Printf.ksprintf failwith fmt

let t = Hashtbl.create 10

let () = List.iter (fun (id, num) -> Hashtbl.replace t id num)
  [ ("e", Float.exp 1.0)
  ; ("pi", Float.pi)
  ]

let define id num = Hashtbl.replace t id num

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

let rec eval = function
	| Expr e -> Some (eval_expr e)
	| Define (id, e) -> define id (eval_expr e); None

and eval_expr = function
	| Const x -> x
	| Id id -> apply id []
	| BinOp(x,Add,y) -> eval_expr x +. eval_expr y
	| BinOp(x,Sub,y) -> eval_expr x -. eval_expr y
	| BinOp(x,Div,y) -> eval_expr x /. eval_expr y
	| BinOp(x,Mul,y) -> eval_expr x *. eval_expr y
	| BinOp(x,Pow,y) -> Float.pow (eval_expr x)  (eval_expr y)
	| UnOp(Neg,x) -> -. (eval_expr x)
	| Apply(id, args) -> apply id (List.map eval_expr args)
	
