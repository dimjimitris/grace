open Ast

type 'a gift = { sem : unit -> unit; node : 'a }
type mode = OnlyAst | AstSymbol | AstSymbolSem

let mode = ref AstSymbolSem
let tbl : Symbol.symbol_table = { scopes = []; table = Hashtbl.create 64 }

let wrap_open_scope sym_tbl =
  match !mode with OnlyAst -> () | _ -> Symbol.open_scope sym_tbl

let wrap_close_scope loc sym_tbl =
  match !mode with OnlyAst -> () | _ -> Symbol.close_scope loc sym_tbl

let enjoy { sem; node } =
  sem ();
  node

let wrap_var_def loc (id, t) sym_tbl =
  let node = { loc; node = (id, t) } in
  let sem () =
    match !mode with
    | OnlyAst -> ()
    | AstSymbol -> Sem.ins_var_def node sym_tbl
    | AstSymbolSem ->
        Sem.sem_var_def node sym_tbl;
        Sem.ins_var_def node sym_tbl
  in
  enjoy { sem; node }

let wrap_param_def loc param _ =
  let node = { loc; node = param } in
  node

let wrap_l_value loc lv sym_tbl =
  let node = { loc; node = lv } in
  let sem () =
    match !mode with
    | OnlyAst -> ()
    | AstSymbol -> ()
    | AstSymbolSem -> Sem.sem_l_value node sym_tbl |> ignore
  in
  enjoy { sem; node }

let wrap_expr loc expr sym_tbl =
  let node = { loc; node = expr } in
  let sem () =
    match !mode with
    | OnlyAst -> ()
    | AstSymbol -> ()
    | AstSymbolSem -> Sem.sem_expr node sym_tbl |> ignore
  in
  enjoy { sem; node }

let wrap_func_call loc func_call sym_tbl =
  let node = { loc; node = func_call } in
  let sem () =
    match !mode with
    | OnlyAst -> ()
    | AstSymbol -> ()
    | AstSymbolSem -> Sem.sem_func_call node sym_tbl |> ignore
  in
  enjoy { sem; node }

let wrap_cond loc comp sym_tbl =
  let node = { loc; node = comp } in
  let sem () =
    match !mode with
    | OnlyAst -> ()
    | AstSymbol -> ()
    | AstSymbolSem -> Sem.sem_cond node sym_tbl
  in
  enjoy { sem; node }

let wrap_stmt loc stmt sym_tbl =
  let node = { loc; node = stmt } in
  let sem () =
    match !mode with
    | OnlyAst -> ()
    | AstSymbol -> ()
    | AstSymbolSem -> Sem.sem_stmt node sym_tbl
  in
  enjoy { sem; node }

let wrap_block loc stmt_n_l =
  let node = { loc; node = stmt_n_l } in
  node

let wrap_func_def loc (head_n, local_n_l, blk_n) =
  let node = { loc; node = (head_n, local_n_l, blk_n) } in
  node

let wrap_decl_header loc (id, param_n_list, ret_data) sym_tbl =
  let node = { loc; node = (id, param_n_list, ret_data) } in
  let sem () =
    match !mode with
    | OnlyAst -> ()
    | AstSymbol ->
        Sem.ins_func_decl node sym_tbl;
        wrap_open_scope sym_tbl;
        List.iter (fun p -> Sem.ins_param_def p sym_tbl) param_n_list;
        wrap_close_scope loc sym_tbl
    | AstSymbolSem ->
        Sem.sem_header node sym_tbl;
        Sem.sem_func_decl node sym_tbl;
        Sem.ins_func_decl node sym_tbl;
        wrap_open_scope sym_tbl;
        List.iter
          (fun p ->
            Sem.sem_param_def p sym_tbl;
            Sem.ins_param_def p sym_tbl)
          param_n_list;
        wrap_close_scope loc sym_tbl
  in
  enjoy { sem; node }

let wrap_def_header loc (id, param_n_list, ret_data)
    (sym_tbl : Symbol.symbol_table) =
  let node = { loc; node = (id, param_n_list, ret_data) } in
  let sem () =
    match !mode with
    | OnlyAst -> ()
    | AstSymbol ->
        Sem.ins_func_def node sym_tbl;
        wrap_open_scope sym_tbl;
        List.iter (fun p -> Sem.ins_param_def p sym_tbl) param_n_list
    | AstSymbolSem ->
        Sem.sem_header node sym_tbl;
        Sem.sem_func_def node sym_tbl;
        Sem.ins_func_def node sym_tbl;
        wrap_open_scope sym_tbl;
        List.iter
          (fun p ->
            Sem.sem_param_def p sym_tbl;
            Sem.ins_param_def p sym_tbl)
          param_n_list
  in
  enjoy { sem; node }

let wrap_local_def loc local_n =
  match local_n with
  | `FuncDef fd -> [ { loc; node = FuncDef fd } ]
  | `FuncDecl fd -> [ { loc; node = FuncDecl fd } ]
  | `VarDefList vl -> List.map (fun v -> { loc; node = VarDef v }) vl

let wrap_func_decl loc head_n =
  let node = { loc; node = head_n } in
  node

let wrap_program loc prog_n =
  let node = { loc; node = prog_n } in
  node
