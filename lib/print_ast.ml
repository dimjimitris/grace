open Ast

let sep, endl = "--", "\n"

let pr_enable str enable =
  if enable then str ^ endl else str

let pr_arit off enable arit =
  let str = match arit with
    | Add -> off ^ "Add"
    | Sub -> off ^ "Sub"
    | Mul -> off ^ "Mul"
    | Div -> off ^ "Div"
    | Mod -> off ^ "Mod"
  in pr_enable str enable

let pr_uarit off enable uarit =
  let str = match uarit with
    | Pos -> off ^ "Pos"
    | Neg -> off ^ "Neg"
  in pr_enable str enable

let pr_comp off enable comp =
  let str = match comp with
    | Eq  -> off ^ "Eq"
    | Neq -> off ^ "Neq"
    | Lt  -> off ^ "Lt"
    | Leq -> off ^ "Leq"
    | Gt  -> off ^ "Gt"
    | Geq -> off ^ "Geq"
  in pr_enable str enable

let pr_logic off enable logic =
  let str = match logic with
    | And -> off ^ "And"
    | Or  -> off ^ "Or"
  in pr_enable str enable

let pr_data off enable data =
  let print_dim = function
  | None    -> "None"
  | Some i  -> string_of_int i in
  let rec aux off = function
  | Int           -> off ^ "Int"
  | Char          -> off ^ "Char"
  | Nothing       -> off ^ "Nothing"
  | Array (t, d)  -> off ^ "Array(" ^ aux "" t ^ ", " ^ print_dim d ^ ")" in
  pr_enable (aux off data) enable

let pr_var off enable (id, t) =
  let str = off ^ "Var(" ^ id ^ " : " ^ pr_data "" false t ^ ")" in
  pr_enable str enable

let pr_param off enable (id, t, ref) =
  let str = off ^ "FparDef(" ^ "id: " ^ id ^ ", data_type: " ^ pr_data "" false t ^ ")" ^
  (if ref = ByReference then (endl ^ off ^ "pass by reference") else "") in
  pr_enable str enable
  
let rec pr_l_value off enable lv =
  let str = off ^ match lv with
  | Id id ->
    "Id(" ^ id ^ ")"
  | LString str ->
    "LString(\"" ^ str ^ "\")"
  | ArrayAccess (lv, e) ->
    "ArrayAccess(" ^ endl ^
    pr_l_value off true (get_node lv) ^
    pr_expr off false (get_node e) ^ ")" in
  pr_enable str enable

and pr_expr off enable ex =
  let str = match ex with
  | LitInt i ->
    off ^ "LitInt(" ^ string_of_int i ^ ")"
  | LitChar c ->
    off ^ "LitChar(" ^ Char.escaped c ^ ")"
  | LValue lv ->
    off ^ "LValue(" ^ endl ^
    pr_l_value (off ^ sep) false (get_node lv) ^ ")"
  | EFuncCall fc ->
    off ^ "EFuncCall(" ^ endl ^
    pr_func_call (off ^ sep) false (get_node fc) ^ ")"
  | Signed (op, e) ->
    off ^ "Signed(" ^ endl ^
    pr_uarit (off ^ sep) true op ^
    pr_expr (off ^ sep) false (get_node e) ^ ")"
  | AritOp (e1, op, e2) ->
    off ^ "AritOp(" ^ endl ^
    pr_expr (off ^ sep) true (get_node e1) ^
    pr_arit (off ^ sep) true op ^
    pr_expr (off ^ sep) false (get_node e2) ^ ")"
  in pr_enable str enable

and pr_func_call off enable (id, el) =
  let str = off ^ "FuncCall(" ^ "id: " ^ id ^ ", args: " ^ endl ^
    String.concat
      ""
      (List.mapi (fun i e -> pr_expr (off ^ sep) (i = List.length el - 1) (get_node e)) el) ^
    ")" in
  pr_enable str enable

let rec pr_cond off enable cond =
  let str = match cond with
  | Comp (e1, cond, e2) ->
    off ^ "Comp(" ^ endl ^
    pr_expr (off ^ sep) true (get_node e1)  ^
    pr_comp (off ^ sep) true cond ^
    pr_expr (off ^ sep) false (get_node e2)  ^ ")"
  | Logic (c1, log, c2) ->
    off ^ "Logic(" ^ endl ^
    pr_cond (off ^ sep) true (get_node c1)  ^
    pr_logic (off ^ sep) true log ^
    pr_cond (off ^ sep) false (get_node c2)  ^ ")"
  | Not c ->
    off ^ "Not(" ^ endl ^
    pr_cond (off ^ sep) false (get_node c) ^ ")"
  in pr_enable str enable

let rec pr_stmt off enable stmt =
  let str = match stmt with
    | Empty -> off ^ "Empty"
    | Assign (lv, e) ->
      off ^ "Assign: " ^ endl ^
      pr_l_value (off ^ sep) true (get_node lv) ^
      pr_expr (off ^ sep) false (get_node e)
    | Block b ->
      pr_block off enable (get_node b)
    | SFuncCall fc ->
      off ^ "SFuncCall: " ^ endl ^
      pr_func_call (off ^ sep) false (get_node fc)
    | If (c, s1, s2) ->
      off ^ "If: " ^ endl ^
      pr_cond (off ^ sep) true (get_node c )^
      (match s1 with
      | None -> ""
      | Some s -> pr_stmt (off ^ sep) (Option.is_some s2) (get_node s)      
      ) ^
      (match s2 with
      | None -> ""
      | Some s -> pr_stmt (off ^ sep) false (get_node s)
      )
    | While (c, s) ->
      off ^ "While: " ^ endl ^
      pr_cond (off ^ sep) true (get_node c) ^
      pr_stmt (off ^ sep) false (get_node s)
    | Return e ->
      off ^ "Return: " ^ (if Option.is_some e then endl else "") ^
      (match e with
      | None -> ""
      | Some e -> pr_expr (off ^ sep) false (get_node e)
      )
  in pr_enable str enable

and pr_block off enable stmts =
  let str = off ^ "Block: " ^ endl ^
    String.concat "" (List.mapi (fun i s -> pr_stmt (off ^ sep) (i = List.length stmts - 1) (get_node s)) stmts) in
  pr_enable str enable

let pr_header off enable ((id, pl, data) : header) =
  let str = off ^ "Header(" ^ endl ^ off ^ sep ^ "id: " ^ id ^ endl ^ off ^ sep ^ "fpar_defs: " ^ endl ^
    String.concat "" (List.mapi (fun i f -> pr_param (off ^ sep ^ sep) (i = List.length pl - 1) (get_node f)) pl) ^
    off ^ sep ^ "data_type: " ^ pr_data "" false data ^ ")"
  in pr_enable str enable

let pr_func_decl off enable head =
  let str = off ^ "FuncDecl(" ^ endl ^
    pr_header (off ^ sep) false (get_node head) ^ ")" in
  pr_enable str enable

let rec pr_func_def off enable (head, ldl, block) =
  let str = off ^ "FuncDef(" ^ endl ^
    pr_header (off ^ sep) true (get_node head) ^
    String.concat "" (List.mapi (fun i l -> pr_local_def (off ^ sep) (i = List.length ldl - 1) (get_node l)) ldl) ^
    pr_block (off ^ sep) false (get_node block) ^ ")" in
  pr_enable str enable

and pr_local_def off enable ld =
  let str = match ld with
    | FuncDef fd -> off ^ "FuncDef: " ^ endl ^ pr_func_def (off ^ sep) false (get_node fd)
    | FuncDecl fd -> off ^ "FuncDecl: " ^ endl ^ pr_func_decl (off ^ sep) false (get_node fd)
    | Var vd -> off ^ "Var: " ^ endl ^ pr_var (off ^ sep) true (get_node vd) in
  pr_enable str enable