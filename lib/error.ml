type loc = Lexing.position * Lexing.position

exception Lexing_error of loc * string
exception Semantic_error of loc * string
exception Symbol_table_error of loc * string
exception Internal_compiler_error of string
exception Codegen_error of loc * string

let string_of_loc
    ( {
        Lexing.pos_fname = filename;
        Lexing.pos_lnum = line;
        Lexing.pos_bol;
        Lexing.pos_cnum;
      },
      _ ) =
  let col = pos_cnum - pos_bol + 1 in
  Printf.sprintf "file: %s, line: %d, column: %d" filename line col

let pr_lexing_error (loc, msg) =
  prerr_endline ("Lexing error at " ^ string_of_loc loc ^ ": " ^ msg)

let pr_parser_error (loc, msg) =
  prerr_endline ("Parser error at " ^ string_of_loc loc ^ ": " ^ msg)

let pr_semantic_error (loc, msg) =
  prerr_endline ("Semantic error at " ^ string_of_loc loc ^ ": " ^ msg)

let pr_symbol_table_error (loc, msg) =
  prerr_endline ("Symbol table error at " ^ string_of_loc loc ^ ": " ^ msg)

let pr_internal_compiler_error msg = prerr_endline ("Internal compiler error " ^ msg)

let pr_codegen_error (loc, msg) =
  prerr_endline ("Codegen error at " ^ string_of_loc loc ^ ": " ^ msg)