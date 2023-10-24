open Error

type func_status = Defined | Declared

type entry_type =
  | Variable of Ast.var_def
  | Parameter of Ast.param_def
  | Function of { header : Ast.header; mutable status : func_status }

type entry = { id : string; info : entry_type }

type internal_entry = { entry : entry; scope : scope ref }
and scope = { mutable entries : internal_entry list }

type symbol_table = {
  mutable scopes : scope list;
  table : (string, internal_entry) Hashtbl.t;
}

let entry_type { info; _ } =
  match info with
  | Variable (_, t) -> t
  | Parameter (_, t, _) -> t
  | Function { header = _, _, data; _ } -> data

let insert loc (entry : entry) (sym_tbl : symbol_table) =
  match sym_tbl.scopes with
  | [] ->
      raise
        (Symbol_table_error (loc, "Tried to insert into empty symbol table"))
  | hd :: _ ->
      let internal_entry = { entry; scope = ref hd } in
      Hashtbl.add sym_tbl.table entry.id internal_entry;
      hd.entries <- internal_entry :: hd.entries

let lookup id sym_tbl =
  match sym_tbl.scopes with
  | [] -> None
  | hd :: _ -> (
      match Hashtbl.find_opt sym_tbl.table id with
      | Some { entry; scope } -> if !scope == hd then Some entry else None
      | None -> None)

let lookup_all id sym_tbl =
  match sym_tbl.scopes with
  | [] -> None
  | _ -> (
      match Hashtbl.find_opt sym_tbl.table id with
      | Some { entry; _ } -> Some entry
      | None -> None)

let open_scope sym_tbl = sym_tbl.scopes <- { entries = [] } :: sym_tbl.scopes

let close_scope loc sym_tbl =
  match sym_tbl.scopes with
  | [] -> raise (Symbol_table_error (loc, "Tried to close empty symbol table"))
  | hd :: tl ->
      List.iter
        (fun { entry; _ } -> Hashtbl.remove sym_tbl.table entry.id)
        hd.entries;
      sym_tbl.scopes <- tl
