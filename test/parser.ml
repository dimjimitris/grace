open Grace_lib

let () =
  let dirname = "programs/" in
  let filenames =
    [ "program12.grc" ]
  in
  let test filename =
    let chan = open_in (dirname ^ filename) in
    let lexbuf = Lexing.from_channel chan in
    Lexing.set_filename lexbuf filename;
    try
      let ast_node = Parser.program Grace_lib.Lexer.token lexbuf in
      Ast.get_node ast_node |> Print_ast.pr_program |> print_string
    with
    | Error.Lexing_error (loc, msg) ->
        Error.pr_lexing_error (loc, msg);
        Print_symbol.pr_symbol_table "" true Gift.tbl |> print_endline
    | Error.Semantic_error (loc, msg) ->
        Error.pr_semantic_error (loc, msg);
        Print_symbol.pr_symbol_table "" true Gift.tbl |> print_endline
    | Error.Symbol_table_error (loc, msg) ->
        Error.pr_symbol_table_error (loc, msg);
        Print_symbol.pr_symbol_table "" true Gift.tbl |> print_endline
  in
  List.iter test filenames
