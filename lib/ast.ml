type loc = Lexing.position * Lexing.position

(* parser rules will return <something> node types *)
type 'a node = { loc : loc; node : 'a }

let get_loc n = n.loc
let get_node n = n.node

type data_type = Int | Char | Nothing | Array of data_type * int option
type un_arit_op = Pos | Neg
type bin_arit_op = Add | Sub | Mul | Div | Mod
type un_logic_op = Not
type bin_logic_op = And | Or
type comp_op = Eq | Neq | Gt | Lt | Geq | Leq
type var_type = data_type
type param_type = data_type
type ret_type = data_type
type pass_by = Value | Reference
type var_def = string * data_type
type param_def = string * data_type * pass_by

type l_value =
  | Id of string
  | LString of string
  | ArrayAccess of l_value node * expr node

and func_call = string * expr node list

and expr =
  | LitInt of int
  | LitChar of char
  | LValue of l_value node
  | EFuncCall of func_call node
  | UnAritOp of un_arit_op * expr node
  | BinAritOp of expr node * bin_arit_op * expr node

type cond =
  | UnLogicOp of un_logic_op * cond node
  | BinLogicOp of cond node * bin_logic_op * cond node
  | CompOp of expr node * comp_op * expr node

type block = stmt node list

and stmt =
  | Empty
  | Assign of l_value node * expr node
  | Block of block node
  | SFuncCall of func_call node
  | If of cond node * stmt node option * stmt node option
  | While of cond node * stmt node
  | Return of expr node option

type header = string * param_def node list * data_type
type func_decl = header node

type func_def = header node * local_def node list * block node

and local_def =
  | FuncDef of func_def node
  | FuncDecl of func_decl node
  | VarDef of var_def node

type program = MainFunc of func_def node

(* creates an Ast.var_type instance from an Ast.data_type and an int list *)
let rec create_var_type dt dims =
  match dims with [] -> dt | hd :: tl -> Array (create_var_type dt tl, Some hd)

(* creates an Ast.param_type instance from an Ast.data_type, an int list and a bool flag *)
let create_param_type dt dims flexible =
  match (dims, flexible) with
  | [], false -> dt
  | hd :: tl, false -> Array (create_var_type dt tl, Some hd)
  | dims, true -> Array (create_var_type dt dims, None)

let rec l_value_dep_on_l_string = function
  | Id _ -> false
  | LString _ -> true
  | ArrayAccess (lv, _) -> l_value_dep_on_l_string (get_node lv)
