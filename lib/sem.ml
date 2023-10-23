open Ast
open Symbol
open Error

(* ensures that defined variables conform to the language's spec *)
let verify_var ({ loc; node = _, dt } : var node) =
  let rec aux = function
    | Array (_, None) ->
        raise (Grace_error(Semantic_error, (loc, "Missing array size is not permitted here")))
    | Array (t, _) -> aux t
    | _ -> ()
  in
  aux dt

(* ensures that defined function parameters conform to the language's spec *)
let verify_param ({ loc; node = id, t, _ } : param node) =
  match t with
  | Array (t, None) -> verify_var { loc; node = (id, t) }
  | t -> verify_var { loc; node = (id, t) }

let comp_var_param va par =
  verify_var va;
  verify_param par;
  match (va, par) with
  | { node = _, t1; _ }, { node = _, t2, _; _ } -> (
      match (t1, t2) with
      | Array (t1, _), Array (t2, None) -> t1 = t2
      | _ -> t1 = t2)

(*
  1. verify that the variable type is well-defined
  2. verify that the variable is not defined twice in the same scope
*)
let sem_var ({ loc; node = id, t } : var node) sym_tbl =
  verify_var { loc; node = (id, t) };
  match lookup id sym_tbl with
  | Some _ -> raise (Grace_error(Semantic_error, (loc, "Variable already declared")))
  | None -> ()


let ins_var ({loc; node = id, t } : var node) sym_tbl =
  insert loc { id; info = Variable (id, t) } sym_tbl

(*
  1. verify that function parameter type is well-defined
  2. verify that array parameters are passed by reference
  3. verify that function parameter is not defined twice in the same function
*)
let sem_param ({ loc; node = id, t, pass } as param : param node) sym_tbl =
  verify_param param;
  let check_type () =
    match (t, pass) with
    | Array _, ByValue ->
        raise (Grace_error(Semantic_error, (loc, "Array must be passed by reference")))
    | _ -> ()
  in
  check_type ();
  match lookup id sym_tbl with
  | Some _ -> raise (Grace_error(Semantic_error, (loc, "Parameter already declared")))
  | None -> ()

let ins_param ({ loc; node = id, t, pass } : param node) sym_tbl =
  insert loc { id; info = Parameter (id, t, pass) } sym_tbl

(*
  1. verify that lvalue exists in the symbol table (unless it's a string)
  2. verify that array access expression evaluates to int
*)
let rec sem_l_value ({ loc; node = lv } : l_value node) sym_tbl =
  match lv with
  | Id id -> (
      match lookup_all id sym_tbl with
      | None ->
          raise (Grace_error(Semantic_error, (loc, "Variable not defined in any scope")))
      | Some entry -> entry_type entry)
  | LString ls -> Array (Char, Some (String.length ls))
  | ArrayAccess (lv, e) ->
      if sem_expr e sym_tbl = Int then
        match sem_l_value lv sym_tbl with
        | Array (t, _) -> t
        | _ ->
            raise (Grace_error(Semantic_error, (loc, "Array access must be of type array")))
      else
        raise (Grace_error(Semantic_error, (loc, "Array access index must be of type int")))

and sem_expr ({ loc; node = ex } : expr node) sym_tbl =
  match ex with
  | LitInt _ -> Int
  | LitChar _ -> Char
  | LValue lv -> sem_l_value lv sym_tbl
  | EFuncCall fc -> sem_func_call fc sym_tbl
  | Signed (_, e) ->
      if sem_expr e sym_tbl = Int then Int
      else raise (Grace_error(Semantic_error, (loc, "Signed expression must be of type int")))
  | AritOp (e1, _, e2) ->
      if sem_expr e1 sym_tbl = Int && sem_expr e2 sym_tbl = Int then Int
      else
        raise (Grace_error(Semantic_error, (loc, "Arithmetic operators must be of type int")))

and sem_func_call ({ loc; node = id, el } : func_call node) sym_tbl =
  let rec check_params (args : expr node list) (param_list : param node list) =
    match (args, param_list) with
    | [], [] -> true
    | [], _ -> raise (Grace_error(Semantic_error, (loc, "Too few arguments")))
    | _, [] -> raise (Grace_error(Semantic_error, (loc, "Too many arguments")))
    | arg :: args, param :: params ->
        if
          comp_var_param
            { loc = arg.loc; node = ("", sem_expr arg sym_tbl) }
            param
        then check_params args params
        else raise (Grace_error(Semantic_error, (loc, "Argument type mismatch")))
  in
  match lookup_all id sym_tbl with
  | None -> raise (Grace_error(Semantic_error, (loc, "Function not defined in any scope")))
  | Some { info; _ } -> (
      match info with
      | Function { header = _, params, data; _ } ->
          if check_params el params then data
          else raise (Grace_error(Semantic_error, (loc, "Function call type mismatch")))
      | _ -> raise (Grace_error(Semantic_error, (loc, "Name is not a function"))))

let sem_cond ({ loc; node } : cond node) sym_tbl =
  match node with
  | Comp (e1, _, e2) ->
      let aux e1 e2 tt = sem_expr e1 sym_tbl = tt && sem_expr e2 sym_tbl = tt in
      if aux e1 e2 Int || aux e1 e2 Char then ()
      else
        raise (Grace_error(Semantic_error, (loc, "Comparison must be of type int or char")))
  | _ -> ()

let sem_stmt ({ loc; node } : stmt node) sym_tbl =
  match node with
  | Assign (lv, e) ->
      let aux t1 t2 tt = t1 = tt && t2 = tt in
      let t1 = sem_l_value lv sym_tbl in
      let t2 = sem_expr e sym_tbl in
      if l_value_dep_on_l_string lv.node
      then raise (Grace_error(Semantic_error, (loc, "Can't assign to a string")))
      else if aux t1 t2 Int || aux t1 t2 Char then ()
      else raise (Grace_error(Semantic_error, (loc, "Assignment type mismatch")))
  | Return ex -> (
      let ex_typ =
        match ex with None -> Nothing | Some e -> sem_expr e sym_tbl
      in
      let sc = List.hd (List.tl !sym_tbl) in
      let entry = List.hd sc.entries in
      match entry.info with
      | Function _ ->
          if ex_typ = entry_type entry then ()
          else raise (Grace_error(Semantic_error, (loc, "Return type mismatch")))
      | _ -> raise (Grace_error(Semantic_error, (loc, "Return statement outside function"))))
  | _ -> ()

let sem_header ({ loc; node = _, _, data } : header node) _ =
  match data with
  | Array _ -> raise (Grace_error(Semantic_error, (loc, "Return type can't be an array")))
  | _ -> ()

let sem_func_decl
    ({ loc; node = (id, _, _) } : header node) sym_tbl
    =
  match lookup id sym_tbl with
  | Some _ -> raise (Grace_error(Semantic_error, (loc, "Function already declared")))
  | None -> ()

let ins_func_decl ({ loc; node = (id, _, _) as head} : header node) sym_tbl =
  insert
    loc
    { id; info = Function { header = head; status = Declared } }
    sym_tbl

let compare_heads ((id1, pl1, d1) : header) ((id2, pl2, d2) : header) =
  let skin params = List.map (fun { node = _, t, p; _ } -> (t, p)) params in
  let p1, p2 = (skin pl1, skin pl2) in
  id1 = id2 && p1 = p2 && d1 = d2

let sem_func_def ({ loc; node = h } : header node) sym_tbl =
  match h with
  | id, _, _ -> 
      match lookup id sym_tbl with
      | None -> ()
      | Some { info; _ } -> (
          match info with
          | Function ({ header; _ } as f) ->
              if f.status = Defined then
                raise (Grace_error(Semantic_error, (loc, "Function already defined")))
              else if compare_heads h header then f.status <- Defined
              else
                raise
                  (Grace_error(Semantic_error,
                     (loc, "Function declaration and definition mismatch")))
          | _ -> raise (Grace_error(Semantic_error, (loc, "Name is not a function"))))

let ins_func_def ({ loc; node = h } : header node) sym_tbl =
  match h with
  | id, _, _ -> 
      match lookup id sym_tbl with
      | None -> 
        insert
          loc
          { id; info = Function { header = h; status = Defined } }
          sym_tbl
      | Some _ -> ()
