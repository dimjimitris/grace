type loc = Error.loc

type scalar = Int | Char | Nothing
type data_type = Scalar of scalar | Array of scalar * int option list

type un_arit_op = Pos | Neg
type bin_arit_op = Add | Sub | Mul | Div | Mod
type un_logic_op = Not
type bin_logic_op = And | Or
type comp_op = Eq | Neq | Gt | Lt | Geq | Leq

type var_type = data_type
type param_type = data_type
type ret_type = scalar
type pass_by = Value | Reference
type func_status = Defined | Declared

type var_def = {
  id : string;
  type_t : var_type;
  frame_offset : int;
  parent_path : string list;
  (* depth : int; *)
  loc : loc;
}

type param_def = {
  id : string;
  type_t : param_type;
  pass_by : pass_by;
  mutable frame_offset : int;
  mutable parent_path : string list;
  (* mutable depth : int; *)
  loc : loc;
}

type l_value_id = {
  id : string;
  mutable type_t : data_type;
  mutable passed_by : pass_by;
  mutable frame_offset : int;
  mutable parent_path : string list;
  (* mutable depth : int; *)
  loc : loc;
}

type l_value_lstring = {
  id : string;
  type_t : data_type;
  loc : loc;
}

type l_value_array_access = {
  simple_l_value : simple_l_value;
  exprs : expr list;
  loc : loc;
}

and simple_l_value =
  | Id of l_value_id
  | LString of l_value_lstring

and l_value =
  | Simple of simple_l_value
  | ArrayAccess of l_value_array_access

and func_call = {
  id : string;
  mutable args : (expr * pass_by) list;
  mutable type_t : ret_type;
  mutable callee_path : string list;
  (* mutable callee_depth : int; *)
  caller_path : string list;
  (* caller_depth : int; *)
  loc : loc;
}

and expr =
  | LitInt of { value : int; loc : loc }
  | LitChar of { value : char; loc : loc }
  | LValue of l_value
  | EFuncCall of func_call
  | UnAritOp of un_arit_op * expr
  | BinAritOp of expr * bin_arit_op * expr

type cond =
  | UnLogicOp of un_logic_op * cond
  | BinLogicOp of cond * bin_logic_op * cond
  | CompOp of expr * comp_op * expr

type block = {
  stmts : stmt list;
  loc : loc;
}

and stmt =
  | Empty of loc
  | Assign of l_value * expr
  | Block of block
  | SFuncCall of func_call
  | If of cond * stmt option * stmt option
  | While of cond * stmt
  | Return of { expr_o : expr option; loc : loc }

type func = {
  id : string;
  params : param_def list;
  type_t : ret_type;
  mutable local_defs : local_def list;
  mutable body : block option;
  loc : loc;
  parent_path : string list;
  (*depth : int; *)
  status : func_status;
}

and local_def = VarDef of var_def | FuncDecl of func | FuncDef of func

type program = MainFunc of func

let get_loc_simple_l_value = function
  | Id id -> id.loc
  | LString lstring -> lstring.loc  

let get_loc_l_value = function
  | Simple simple_l_value -> get_loc_simple_l_value simple_l_value
  | ArrayAccess array_access -> array_access.loc

let rec get_loc_expr = function
  | LitInt lit_int -> lit_int.loc
  | LitChar lit_char -> lit_char.loc
  | LValue l_value -> get_loc_l_value l_value
  | EFuncCall func_call -> func_call.loc
  | UnAritOp (_, expr) -> get_loc_expr expr
  | BinAritOp (expr1, _, _) -> get_loc_expr expr1

let rec get_loc_cond = function
  | UnLogicOp (_, cond) -> get_loc_cond cond
  | BinLogicOp (cond1, _, _) -> get_loc_cond cond1
  | CompOp (expr1, _, _) -> get_loc_expr expr1

let get_loc_stmt = function
  | Empty loc -> loc
  | Assign (l_value, _) -> get_loc_l_value l_value
  | Block block -> block.loc
  | SFuncCall func_call -> func_call.loc
  | If (cond, _, _) -> get_loc_cond cond
  | While (cond, _) -> get_loc_cond cond
  | Return { loc; _} -> loc

let get_loc_local_def = function
  | VarDef var_def -> var_def.loc
  | FuncDecl func -> func.loc
  | FuncDef func -> func.loc

let get_loc_program = function
  | MainFunc func -> func.loc

  let create_var_type dt dims =
    match dims with [] -> Scalar dt | _ -> Array (dt, List.map (fun x -> Some x) dims)

let create_param_type dt dims flexible =
  match (dims, flexible) with
  | [], false -> Scalar dt
  | (_ :: _ as dims), false -> Array (dt, List.map (fun x -> Some x) dims)
  | dims, true -> Array (dt, None :: List.map (fun x -> Some x) dims)

let l_string_dependence l_v = 
  let aux = function Id _ -> false | LString _ -> true in
  match l_v with
  | Simple simple_l_value -> aux simple_l_value
  | ArrayAccess { simple_l_value; _ } -> aux simple_l_value

let reorganize_local_defs (local_defs : local_def list) =
  let rec reorganize_local_defs' (local_defs : local_def list)
      (vars : var_def list) (decls : func list) (funcs : func list) =
    match local_defs with
    | [] -> (vars, decls, funcs)
    | VarDef v :: local_defs ->
        reorganize_local_defs' local_defs (v :: vars) decls funcs
    | FuncDecl d :: local_defs ->
        reorganize_local_defs' local_defs vars (d :: decls) funcs
    | FuncDef f :: local_defs ->
        reorganize_local_defs' local_defs vars decls (f :: funcs)
  in
  let vars, decls, funcs = reorganize_local_defs' local_defs [] [] [] in
  (List.rev vars, List.rev decls, List.rev funcs)

  let get_parent_name (func : func) =
    String.concat "." (List.rev func.parent_path)
  
let get_func_name (func : func) = get_parent_name func ^ "." ^ func.id
let get_parent_frame_name (func : func) = "frame__" ^ get_parent_name func
let get_frame_name (func : func) = "frame__" ^ get_func_name func

let get_proper_parent_func_name (func : func) =
  "func__" ^ get_parent_name func

let get_proper_func_name (func : func) = "func__" ^ get_func_name func

let get_proper_func_call_name (func_call : func_call) =
  "func__"
  ^ String.concat "." (List.rev func_call.callee_path)
  ^ "." ^ func_call.id