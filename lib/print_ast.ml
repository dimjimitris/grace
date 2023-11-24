open Ast

let sep, endl = ("--", "\n")
let pr_enable str enable = if enable then str ^ endl else str

let pr_scalar_type off enable = function
  | Int -> pr_enable (off ^ "int") enable
  | Char -> pr_enable (off ^ "char") enable
  | Nothing -> pr_enable (off ^ "nothing") enable

let pr_data_type off enable = function
  | Scalar t -> pr_scalar_type off enable t
  | Array (t, dims) ->
      let extract_or_none d =
        match d with Some d -> string_of_int d | None -> ""
      in
      let dims = List.map (fun d -> "[" ^ extract_or_none d ^ "]") dims in
      pr_enable (off ^ pr_scalar_type "" false t ^ String.concat "" dims) enable

let pr_un_arit_op off enable = function
  | Pos -> pr_enable (off ^ "pos") enable
  | Neg -> pr_enable (off ^ "neg") enable

let pr_bin_arit_op off enable = function
  | Add -> pr_enable (off ^ "add") enable
  | Sub -> pr_enable (off ^ "sub") enable
  | Mul -> pr_enable (off ^ "mul") enable
  | Div -> pr_enable (off ^ "div") enable
  | Mod -> pr_enable (off ^ "mod") enable

let pr_bin_logic_op off enable = function
  | And -> pr_enable (off ^ "and") enable
  | Or -> pr_enable (off ^ "or") enable

let pr_comp_op off enable = function
  | Eq -> pr_enable (off ^ "eq") enable
  | Neq -> pr_enable (off ^ "neq") enable
  | Lt -> pr_enable (off ^ "lt") enable
  | Leq -> pr_enable (off ^ "leq") enable
  | Gt -> pr_enable (off ^ "gt") enable
  | Geq -> pr_enable (off ^ "geq") enable

let pr_var_def off enable (vd : var_def) =
  let str =
    off ^ "var_def(" ^ vd.id ^ " : "
    ^ pr_data_type "" false vd.type_t
    ^ "): ["
    ^ String.concat "_" (List.rev vd.parent_path)
    ^ "]" ^ " offset: "
    ^ string_of_int vd.frame_offset
  in
  pr_enable str enable

let pr_param_def off enable (pd : param_def) =
  let str =
    off ^ "param_def("
    ^ (if pd.pass_by = Reference then "ref " else "")
    ^ pd.id ^ " : "
    ^ pr_data_type "" false pd.type_t
    ^ "): ["
    ^ String.concat "_" (List.rev pd.parent_path)
    ^ "]" ^ " offset: "
    ^ string_of_int pd.frame_offset
  in
  pr_enable str enable

let pr_simple_l_value off enable (slv : simple_l_value) =
  match slv with
  | Id { id; type_t; passed_by; frame_offset; parent_path;_ } ->
    let str =
      off ^ "id("
      ^ (if passed_by = Reference then "passed by ref " else "")
      ^ id ^ " : "
      ^ pr_data_type "" false type_t
      ^ "): ["
      ^ String.concat "_" (List.rev parent_path)
      ^ "]" ^ " offset: " ^ string_of_int frame_offset
    in
    pr_enable str enable
| LString { id; type_t; _ } ->
    let str =
      off ^ "l_string(" ^ id ^ " : " ^ pr_data_type "" false type_t ^ ")"
    in
    pr_enable str enable

let rec pr_l_value off enable (lv : l_value) =
  match lv with
  | Simple slv -> pr_simple_l_value off enable slv
  | ArrayAccess {simple_l_value=lval; exprs;_} ->
      let str =
        off ^ "array_access(" ^ pr_simple_l_value "" false lval ^ endl
        ^ String.concat ""
            (List.mapi
               (fun i e -> pr_expr (off ^ sep) (i <> List.length exprs - 1) e)
               exprs)
        ^ ")"
      in
      pr_enable str enable

and pr_func_call off enable (fc : func_call) =
  let str =
    off ^ "func_call(" ^ fc.id ^ " : args(" ^ endl
    ^ String.concat ""
        (List.mapi
           (fun i (e, pb) ->
             off ^ sep
             ^ (if pb = Reference then "by reference" else "by value")
             ^ ": " ^ endl
             ^ pr_expr (off ^ sep ^ sep) (i <> List.length fc.args - 1) e)
           fc.args)
    ^ ")" ^ endl ^ off ^ sep ^ "callee["
    ^ String.concat "_" (List.rev fc.callee_path)
    ^ "] " ^ "caller["
    ^ String.concat "_" (List.rev fc.caller_path)
    ^ "] return type: "
    ^ pr_scalar_type "" false fc.type_t
    ^ ")"
  in
  pr_enable str enable

and pr_expr off enable = function
  | LitInt { value=lit_int; _ } ->
      pr_enable (off ^ "lit_int(" ^ string_of_int lit_int ^ ")") enable
  | LitChar { value=lit_char; _ } ->
      pr_enable (off ^ "lit_char(" ^ Char.escaped lit_char ^ ")") enable
  | LValue lval -> pr_l_value off enable lval
  | EFuncCall fc -> pr_func_call off enable fc
  | UnAritOp (op, expr) ->
      let str =
        off ^ "un_arit_op(" ^ endl
        ^ pr_un_arit_op (off ^ sep) true op
        ^ pr_expr (off ^ sep) false expr
        ^ ")"
      in
      pr_enable str enable
  | BinAritOp (e1, op, e2) ->
      let str =
        off ^ "BinAritOp(" ^ endl
        ^ pr_expr (off ^ sep) true e1
        ^ pr_bin_arit_op (off ^ sep) true op
        ^ pr_expr (off ^ sep) false e2
        ^ ")"
      in
      pr_enable str enable

let rec pr_cond off enable cond =
  let str =
    match cond with
    | CompOp (e1, cond, e2) ->
        off ^ "Comp(" ^ endl
        ^ pr_expr (off ^ sep) true e1
        ^ pr_comp_op (off ^ sep) true cond
        ^ pr_expr (off ^ sep) false e2
        ^ ")"
    | BinLogicOp (c1, log, c2) ->
        off ^ "Logic(" ^ endl
        ^ pr_cond (off ^ sep) true c1
        ^ pr_bin_logic_op (off ^ sep) true log
        ^ pr_cond (off ^ sep) false c2
        ^ ")"
    | UnLogicOp (op, c) -> (
        match op with
        | Not -> off ^ "Not(" ^ endl ^ pr_cond (off ^ sep) false c ^ ")")
  in
  pr_enable str enable

let rec pr_block off enable block =
  let {stmts;_} = block in
  let str =
    off ^ "Block: " ^ endl
    ^ String.concat ""
        (List.mapi
           (fun i s -> pr_stmt (off ^ sep) (i <> List.length stmts - 1) s)
           stmts)
  in
  pr_enable str enable

and pr_stmt off enable stmt =
  let str =
    match stmt with
    | Empty _ -> off ^ "Empty"
    | Assign (lv, e) ->
        off ^ "Assign: " ^ endl
        ^ pr_l_value (off ^ sep) true lv
        ^ pr_expr (off ^ sep) false e
    | Block b -> pr_block off enable b
    | SFuncCall fc ->
        off ^ "SFuncCall: " ^ endl ^ pr_func_call (off ^ sep) false fc
    | If (c, s1, s2) -> (
        off ^ "If: " ^ endl
        ^ pr_cond (off ^ sep) true c
        ^ (match s1 with
          | None -> ""
          | Some s -> pr_stmt (off ^ sep) (Option.is_some s2) s)
        ^ match s2 with None -> "" | Some s -> pr_stmt (off ^ sep) false s)
    | While (c, s) ->
        off ^ "While: " ^ endl
        ^ pr_cond (off ^ sep) true c
        ^ pr_stmt (off ^ sep) false s
    | Return { expr_o = e; _ } -> (
        off ^ "Return: "
        ^ (if Option.is_some e then endl else "")
        ^ match e with None -> "" | Some e -> pr_expr (off ^ sep) false e)
  in
  pr_enable str enable

let pr_header off enable (func : func) =
  let str =
    off ^ "Header(" ^ endl ^ off ^ sep ^ "id: " ^ func.id ^ endl ^ off ^ sep
    ^ "fpar_defs: " ^ endl
    ^ String.concat ""
        (List.map (fun f -> pr_param_def (off ^ sep ^ sep) true f) func.params)
    ^ off ^ sep ^ "data_type: "
    ^ pr_scalar_type "" false func.type_t
    ^ ")"
  in
  pr_enable str enable

let pr_func_decl off enable (func : func) =
  let str =
    off ^ "FuncDecl(" ^ endl
    ^ pr_header (off ^ sep) false func
    ^ ")" ^ endl ^ off ^ sep ^ "parent["
    ^ String.concat "_" (List.rev func.parent_path)
    ^ "] status: "
    ^ if func.status = Declared then "declared" else "defined"
  in
  pr_enable str enable

let rec pr_func_def off enable (func : func) =
  let str =
    off ^ "FuncDef(" ^ endl
    ^ pr_header (off ^ sep) false func
    ^ ")" ^ endl ^ off ^ sep ^ "parent["
    ^ String.concat "_" (List.rev func.parent_path)
    ^ "] status: "
    ^ (if func.status = Declared then "declared" else "defined")
    ^ endl
    ^ String.concat ""
        (List.map (fun s -> pr_local_def (off ^ sep) true s) func.local_defs)
    ^ pr_block (off ^ sep) false (Option.get func.body)
  in
  pr_enable str enable

and pr_local_def off enable ld =
  let str =
    match ld with
    | FuncDef fd -> off ^ "FuncDef: " ^ endl ^ pr_func_def (off ^ sep) false fd
    | FuncDecl fd ->
        off ^ "FuncDecl: " ^ endl ^ pr_func_decl (off ^ sep) false fd
    | VarDef vd -> off ^ "Var: " ^ endl ^ pr_var_def (off ^ sep) false vd
  in
  pr_enable str enable

let pr_program prog =
  match prog with
  | MainFunc func_def ->
      let str = "MainFunc(" ^ endl ^ pr_func_def sep true func_def ^ ")" in
      pr_enable str true
