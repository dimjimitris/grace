open Ast
open Symbol
open Error

(* used to compare function declarations with definitions and find undefined declarations *)
module StringSet = Set.Make (String)

(* before closing a scope check for lingering declarations *)
let sem_close_scope loc sym_tbl =
  match sym_tbl.scopes with
  | [] -> raise (Symbol_table_error (loc, "Tried to close empty symbol table"))
  | scope :: _ ->
    (* find declarations and definitions in current scope *)
    let rec aux entries def_set decl_set =
      match entries with
      | [] -> (def_set, decl_set)
      | entry :: tl -> (
          match entry.type_t with
          | Function fdr ->
              let fd = !fdr in
              if fd.status = Declared then
                let decl_set = StringSet.add fd.id decl_set in
                aux tl def_set decl_set
              else
                let def_set = StringSet.add fd.id def_set in
                aux tl def_set decl_set
          | _ -> aux tl def_set decl_set)
    in
    let get_loc_by_id id sym_tbl =
      let entry = lookup id sym_tbl in
      match entry with
      | Some e -> get_loc_entry e
      | None -> raise (Symbol_table_error (loc, "Could not find entry for id " ^ id))
    in
      (* check if all declared functions have been defined *)
    let def_set, decl_set = aux scope.entries StringSet.empty StringSet.empty in
      StringSet.iter
      (fun id ->
        if not (StringSet.mem id def_set) then
          raise
            (Semantic_error
               ( get_loc_by_id id sym_tbl,
                 "Function " ^ id ^ " declared, but not defined" ))) decl_set

let verify_var_def (vd : var_def) =
match vd.type_t with
| Array (_, dims) ->
    if
      List.find_opt
        (fun dim -> match dim with None -> false | Some n -> n <= 0)
        dims
      <> None
    then raise (Semantic_error (vd.loc, "Array dimension must be positive"))
    else if
      List.find_opt
        (fun dim -> match dim with None -> true | _ -> false)
        dims
      <> None
    then raise (Semantic_error (vd.loc, "Array dimension must be specified"))
    else ()
| _ -> ()

let verify_param_def (pd : param_def) =
match pd.type_t with
| Array (_, dims) ->
    let tl_dims = List.tl dims in
    if
      List.find_opt
        (fun dim -> match dim with None -> false | Some n -> n <= 0)
        dims (* check all dimensions *)
      <> None
    then raise (Semantic_error (pd.loc, "Array dimension must be positive"))
    else if
      List.find_opt
        (fun dim -> match dim with None -> true | _ -> false)
        tl_dims
      (* don't check the first dimension, because it can be 'None' *)
      <> None
    then raise (Semantic_error (pd.loc, "Array dimension must be specified"))
    else ()
| _ -> ()

let comp_var_param_types loc (vt : var_type) (pt : param_type) =
  match (vt, pt) with
  | Array (t1, dims1), Array (t2, dims2) ->
      if t1 <> t2 then
        raise (Semantic_error (loc, "Array element type mismatch"))
      else if List.length dims1 <> List.length dims2 then
        raise (Semantic_error (loc, "Array dimension count mismatch"))
      else if List.hd dims2 = None then
        let tl_dims1, tl_dims2 = (List.tl dims1, List.tl dims2) in
        List.iter2
          (fun dim1 dim2 ->
            match (dim1, dim2) with
            | None, None -> ()
            | Some n1, Some n2 ->
                if n1 <> n2 then
                  raise (Semantic_error (loc, "Array dimension size mismatch"))
            | _ -> ())
          tl_dims1 tl_dims2
  | t1, t2 -> if t1 <> t2 then raise (Semantic_error (loc, "Type mismatch"))

let comp_var_param_def (vd : var_def) (pd : param_def) =
  comp_var_param_types vd.loc vd.type_t pd.type_t

let comp_param_decl_def (pd1 : param_def) (pd2 : param_def) =
  if pd1.pass_by <> pd2.pass_by then
    raise
      (Semantic_error
          (pd1.loc, "Parameter definition/declaration 'pass by' mismatch"))
  else if pd1.type_t <> pd2.type_t then
    raise
      (Semantic_error (pd1.loc, "Parameter definition/declaration type mismatch"))

let type_of_ret loc sym_tbl =
  let sc = List.hd (List.tl sym_tbl.scopes) in
  let entry = List.hd sc.entries in
  match entry.type_t with
  | Function f -> !f.type_t
  | _ -> raise (Semantic_error (loc, "Tried to get return type of non-function"))

(* only lvalues can be passed by reference, all other expressions can't *)
let check_ref expr pass_by =
  match (expr, pass_by) with
  | LValue _, _ -> ()
  | _, Value -> ()
  | expr, Reference ->
      raise (Semantic_error (get_loc_expr expr, "Passing non-l-value by reference"))

(* compare headers of a declaration and definition to see if they match *)
let compare_heads (decl : func) (def : func) =
  if decl.type_t <> def.type_t then
    raise
      (Semantic_error
         (def.loc, "Function definition/declaration return type mismatch"))
  else if List.length decl.params <> List.length def.params then
    raise
      (Semantic_error
         (def.loc, "Function definition/declaration parameter count mismatch"))
  else List.iter2 comp_param_decl_def decl.params def.params

let sem_var_def (vd : var_def) (sym_tbl : symbol_table) =
  verify_var_def vd;
  match lookup vd.id sym_tbl with
  | Some _ ->
      raise
        (Semantic_error (vd.loc, "Variable already defined in current scope"))
  | None -> ()

let ins_var_def (vd : var_def) (sym_tbl : symbol_table) =
  insert vd.loc vd.id (Variable (ref vd)) sym_tbl

let sem_param_def (pd : param_def) (sym_tbl : symbol_table) =
  verify_param_def pd;
  match (pd.type_t, pd.pass_by) with
  | Array _, Value ->
      raise
        (Semantic_error (pd.loc, "Array parameter must be passed by reference"))
  | _ -> (
      match lookup pd.id sym_tbl with
      | Some _ ->
          raise
            (Semantic_error
                (pd.loc, "Parameter already defined in current scope"))
      | None -> ())

let ins_param_def (pd : param_def) (sym_tbl : symbol_table) =
  insert pd.loc pd.id (Parameter (ref pd)) sym_tbl

let sem_simple_l_value (slv : simple_l_value) (sym_tbl : symbol_table) =
  match slv with
  | Id l_val_id -> (
    match lookup_all l_val_id.id sym_tbl with
    | None ->
      raise
        (Semantic_error (l_val_id.loc, "Variable not defined in any scope"))
    | Some { type_t; _ } -> (
      match type_t with
      | Variable vdr ->
        let vd = !vdr in
        l_val_id.type_t <- vd.type_t;
        l_val_id.passed_by <- Value;
        l_val_id.frame_offset <- vd.frame_offset;
        l_val_id.parent_path <- vd.parent_path;
        l_val_id.type_t
        | Parameter pdr ->
          let pd = !pdr in
          l_val_id.type_t <- pd.type_t;
          l_val_id.passed_by <- pd.pass_by;
          l_val_id.frame_offset <- pd.frame_offset;
          l_val_id.parent_path <- pd.parent_path;
          l_val_id.type_t
          | Function _ ->
            raise
              (Semantic_error
                 (l_val_id.loc, "Function cannot be used as l-value"))
    )
  )
  | LString { type_t; _} -> type_t

let rec sem_l_value (lv : l_value) (sym_tbl : symbol_table) =
  match lv with
  | Simple slv -> sem_simple_l_value slv sym_tbl
  | ArrayAccess { simple_l_value=slv; exprs; loc} ->
    let rec comp_dims_exprs dims exprs loc =
      match dims, exprs with
      | [], [] -> []
      | [], _ -> raise (Semantic_error (loc, "Array access dimension count mismatch"))
      | dims, [] -> dims
      | _ :: tl_dims, expr :: tl_exprs ->
          let lloc = get_loc_expr expr in
          let expr_type = sem_expr expr sym_tbl in
          if expr_type <> Scalar Int then
            raise
              (Semantic_error (lloc, "Array access dimension must be integer"))
          else comp_dims_exprs tl_dims tl_exprs lloc
    in
    let l_val_type = sem_simple_l_value slv sym_tbl in
    (* l_val_type will be the full type of this array *)
    let type_t, dims =
      (* type_t will be the element type of this array *)
      match l_val_type with
      | Array (type_t, dims) -> (type_t, dims)
      | _ ->
          raise
            (Semantic_error (loc, "Trying to access elements of non array"))
    in
    let dims = comp_dims_exprs dims exprs loc in
    match dims with [] -> Scalar type_t | dims -> Array (type_t, dims)

and sem_func_call (func_call : func_call) (exprs : expr list) sym_tbl =
  match lookup_all func_call.id sym_tbl with
  | None -> raise (Semantic_error (func_call.loc, "Function not defined"))
  | Some { type_t; _ } -> (
      match type_t with
      | Function fdr ->
          let fd = !fdr in
          func_call.type_t <- fd.type_t;
          let param_types =
            List.map (fun (pd : param_def) -> pd.type_t) fd.params
          in
          if List.length param_types <> List.length exprs then
            raise (Semantic_error (func_call.loc, "Parameter count mismatch"))
          else
            let expr_types = List.map (fun expr -> sem_expr expr sym_tbl) exprs
            in
            List.iter2
              (comp_var_param_types func_call.loc)
              expr_types param_types;
            func_call.args <-
              List.map2
                (fun (expr : expr) (param : param_def) ->
                  check_ref expr param.pass_by;
                  (expr, param.pass_by))
                exprs fd.params;
            func_call.callee_path <- fd.parent_path;
            Scalar func_call.type_t
      | _ -> raise (Semantic_error (func_call.loc, "Function not defined")))    

(* don't need to do checks again for LValue and FuncCall again *)
and sem_expr (expr : expr) (sym_tbl : symbol_table) =
  match expr with
  | LitInt _ -> Scalar Int
  | LitChar _ -> Scalar Char
  | LValue l_val -> sem_l_value l_val sym_tbl
  | EFuncCall func_call -> Scalar func_call.type_t
  | UnAritOp (_, expr) -> (
      match sem_expr expr sym_tbl with
      | Scalar Int -> Scalar Int
      | _ ->
          raise
            (Semantic_error
               (get_loc_expr expr, "Unary arithmetic operator must be applied to integer")))
  | BinAritOp (lhs, _, rhs) -> (
      match (sem_expr lhs sym_tbl, sem_expr rhs sym_tbl) with
      | Scalar Int, Scalar Int -> Scalar Int
      | _, _ ->
          raise
            (Semantic_error
               (get_loc_expr lhs, "Binary arithmetic operator must be applied to integer")))

let sem_cond cond sym_tbl =
match cond with
| CompOp (lhs, _, rhs) -> (
    match (sem_expr lhs sym_tbl, sem_expr rhs sym_tbl) with
    | Scalar Int, Scalar Int -> ()
    | Scalar Char, Scalar Char -> ()
    | _, _ ->
        raise
          (Semantic_error
              (get_loc_cond cond, "Comparison operator must be applied to integer")))
| _ -> ()

let sem_stmt stmt sym_tbl =
  match stmt with
  | Assign (l_val, expr) -> (
      let l_val_type = sem_l_value l_val sym_tbl in
      let loc = get_loc_stmt stmt in
      let expr_type = sem_expr expr sym_tbl in
      if Ast.l_string_dependence l_val then
        raise (Semantic_error (loc , "Cannot assign to string literal"))
      else if l_val_type <> expr_type then
        raise (Semantic_error (loc, "Type mismatch"))
      else
        match l_val_type with
        | Array _ -> raise (Semantic_error (loc, "Cannot assign to array"))
        | _ -> ())
  | Return { loc; expr_o } ->
      let expr_type =
        match expr_o with
        | None -> Nothing
        | Some expr -> match sem_expr expr sym_tbl with
          | Scalar s -> s
          | _ -> raise (Semantic_error (loc, "Return type must be scalar"))
      in
      let ret_type = type_of_ret loc sym_tbl in
      if ret_type <> expr_type then
        raise (Semantic_error (loc, "Return type mismatch"))
      else ()
  | _ -> ()

let sem_func_decl (func : Ast.func) (sym_tbl : symbol_table) =
  match lookup func.id sym_tbl with
  | Some _ ->
      raise
        (Semantic_error (func.loc, "Function already exists in current scope"))
  | None -> ()

let ins_func_decl (func : Ast.func) (sym_tbl : symbol_table) =
  insert func.loc func.id (Function (ref func)) sym_tbl

let sem_func_def (func : Ast.func) (sym_tbl : symbol_table) =
  match lookup func.id sym_tbl with
  | None -> ()
  | Some { type_t; _ } -> (
      match type_t with
      | Function fdr ->
          let fd = !fdr in
          if fd.status = Ast.Defined then
            raise
              (Semantic_error
                  (func.loc, "Function already defined in current scope"))
          else compare_heads fd func
      | _ -> raise (Semantic_error (func.loc, "Name is not a function")))

let ins_func_def (func : Ast.func) (sym_tbl : symbol_table) =
  insert func.loc func.id (Function (ref func)) sym_tbl

let sem_program (program : Ast.program) (sym_tbl : symbol_table) =
  let main = match program with MainFunc f -> f in
  if List.length main.params <> 0 then
    raise (Semantic_error (main.loc, "Main function cannot have parameters"))
  else if main.type_t <> Nothing then
    raise (Semantic_error (main.loc, "Main function cannot have return type"))
  else
    (* check for any lingering things that might be left behind from parsing... *)
    Hashtbl.iter
      (fun _ value ->
        match value.type_t with
        | Function fdr ->
            let fd = !fdr in
            if fd.status = Ast.Declared then
              raise
                (Symbol_table_error
                    (fd.loc, "Lingering function declaration " ^ fd.id))
            else
              raise
                (Symbol_table_error
                    (fd.loc, "Lingering function definition " ^ fd.id))
        | Variable v ->
            raise
              (Symbol_table_error
                  (!v.loc, "Lingering variable definition " ^ !v.id))
        | Parameter p ->
            raise
              (Symbol_table_error
                  (!p.loc, "Lingering parameter definition " ^ !p.id)))
      sym_tbl.table;
  program