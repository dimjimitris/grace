open Symbol
open Print_ast

let pr_entry_type off enable et =
  match et with
  | Variable var ->
      off ^ "Variable: " ^ endl ^ pr_var_def (off ^ sep) enable var
  | Parameter param ->
      off ^ "Parameter: " ^ endl ^ pr_param_def (off ^ sep) enable param
  | Function { header; _ } ->
      off ^ "Function: " ^ endl ^ pr_header (off ^ sep) enable header

let pr_entry off enable { id; info } =
  off ^ "Entry: " ^ id ^ endl ^ pr_entry_type (off ^ sep) enable info

let pr_scope off enable { entries = in_entries } =
  let str =
    off ^ "Scope: " ^ endl
    ^ String.concat ""
        (List.rev
           (List.mapi
              (fun i in_entry -> pr_entry (off ^ sep) (i <> 0) in_entry.entry)
              in_entries))
  in
  pr_enable str enable

let pr_symbol_table off enable sym_tbl =
  let str =
    off ^ "Symbol Table: " ^ endl
    ^ String.concat ""
        (List.rev
           (List.mapi
              (fun i scope -> pr_scope (off ^ sep) (i <> 0) scope)
              sym_tbl.scopes))
  in
  pr_enable str enable
