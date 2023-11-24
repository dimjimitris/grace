open Arg

let filenames = ref []

let usage_msg =
  "Usage: codegen --file <filename> --files <filename1> <filename2> ..."

let speclist =
  [
    ( "--file",
      String (fun str -> filenames := str :: !filenames),
      "Input file name" );
    ( "--files",
      Rest (fun str -> filenames := str :: !filenames),
      "Input file names" );
  ]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let test filename =
    let chan = open_in filename in
    let lexbuf = Lexing.from_channel chan in
    Lexing.set_filename lexbuf filename;
    try
      print_endline filename;
      let ast_node = Grace_lib.Parser.program Grace_lib.Lexer.token lexbuf in
      Grace_lib.Codegen.gen_all_frame_types ast_node;
      List.iter
        (fun named_type ->
          print_endline (Llvm.string_of_lltype (Llvm.element_type named_type)))
        (Grace_lib.Codegen.get_all_frame_type_ptrs ast_node);

      print_string "\n====================\n\n"
    with
    | Grace_lib.Error.Lexing_error (loc, msg) ->
        Grace_lib.Error.pr_lexing_error (loc, msg);
        Grace_lib.Print_symbol.pr_symbol_table "" true Grace_lib.Wrapper.tbl
        |> print_endline
    | Grace_lib.Parser.Error ->
        (* built-in Menhir error... *)
        Grace_lib.Error.pr_parser_error
          ( (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf),
            "Syntax error" );
        Grace_lib.Print_symbol.pr_symbol_table "" true Grace_lib.Wrapper.tbl
        |> print_endline
    | Grace_lib.Error.Semantic_error (loc, msg) ->
        Grace_lib.Error.pr_semantic_error (loc, msg);
        Grace_lib.Print_symbol.pr_symbol_table "" true Grace_lib.Wrapper.tbl
        |> print_endline
    | Grace_lib.Error.Symbol_table_error (loc, msg) ->
        Grace_lib.Error.pr_symbol_table_error (loc, msg);
        Grace_lib.Print_symbol.pr_symbol_table "" true Grace_lib.Wrapper.tbl
        |> print_endline
    | Grace_lib.Error.Codegen_error (loc, msg) ->
        Grace_lib.Error.pr_codegen_error (loc, msg);
        Grace_lib.Print_symbol.pr_symbol_table "" true Grace_lib.Wrapper.tbl
        |> print_endline
    | Grace_lib.Error.Internal_compiler_error (msg) ->
        Grace_lib.Error.pr_internal_compiler_error msg;
        Grace_lib.Print_symbol.pr_symbol_table "" true Grace_lib.Wrapper.tbl
        |> print_endline
  in

  List.iter test !filenames
