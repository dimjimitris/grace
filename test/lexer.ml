open Grace_lib

let () =
  let dirname = "programs/" in
  let filenames =
    [ "program1.grc"; "program2.grc"; "program3.grc"; "program4.grc" ]
  in
  let test filename =
    let chan = open_in (dirname ^ filename) in
    let lexbuf = Lexing.from_channel chan in
    Lexing.set_filename lexbuf filename;
    let rec tokenize_and_print () =
      try
        (* call the lexer's 'token' rule (tokenize input) *)
        let token = Lexer.token lexbuf in
        (* print found tokens *)
        match token with
        | Tokens.EOF -> ()
        | _ ->
            Lexer_utils.string_of_token token |> print_endline;
            tokenize_and_print ()
      with Error.Lexing_error (loc, msg) -> Error.pr_lexing_error (loc, msg)
    in
    print_endline filename;
    tokenize_and_print ()
  in
  List.iteri
    (fun i f ->
      test f;
      if i < List.length filenames - 1 then print_newline ())
    filenames
