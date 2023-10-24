open Grace_lib

let () =
  let dirname = "programs/" in
  let filenames =
    [ "program1.grc"; "program2.grc"; "program3.grc"; "program4.grc" ]
  in
  let test filename =
    let chan = open_in (dirname ^ filename) in
    let lexbuf = Lexing.from_channel chan in
    let () = Lexing.set_filename lexbuf filename in
    try
      let ast_node = Parser.program Grace_lib.Lexer.token lexbuf in
      print_string (Print_ast.pr_program (Ast.get_node ast_node))
    with
    | Error.Lexing_error (loc, msg) ->
        Error.pr_lexing_error (loc, msg);
        print_endline (Print_symbol.pr_symbol_table "" true Gift.tbl)
    | Error.Semantic_error (loc, msg) ->
        Error.pr_semantic_error (loc, msg);
        print_endline (Print_symbol.pr_symbol_table "" true Gift.tbl)
    | Error.Symbol_table_error (loc, msg) ->
        Error.pr_symbol_table_error (loc, msg);
        print_endline (Print_symbol.pr_symbol_table "" true Gift.tbl)
  in
  List.iter test filenames
