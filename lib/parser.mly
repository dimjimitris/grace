%{
  open Wrapper
%}

%start <Ast.program> program

%type <Ast.scalar> compl_data_type
%type <Ast.scalar> incompl_data_type
%type <Ast.un_arit_op> un_arit_op
%type <Ast.bin_arit_op> bin_arit_op
%type <Ast.un_logic_op> un_logic_op
%type <Ast.bin_logic_op> bin_logic_op
%type <Ast.comp_op> comp_op
%type <Ast.var_type> var_type
%type <Ast.param_type> param_type
%type <Ast.ret_type> ret_type
%type <Ast.pass_by> pass_by
%type <Ast.var_def list> var_def
%type <Ast.param_def list> param_def
%type <Ast.l_value> l_value
%type <Ast.func_call> func_call
%type <Ast.expr> expr
%type <Ast.cond> cond
%type <Ast.block> block
%type <Ast.stmt> stmt
%type <Ast.func> decl_header
%type <Ast.func> def_header
%type <Ast.func> func_decl
%type <Ast.func> func_def
%type <Ast.local_def list> local_def

%%

let compl_data_type :=
  | INT; { Ast.Int }
  | CHAR; { Ast.Char }

let incompl_data_type :=
  | NOTHING; { Ast.Nothing }

let un_arit_op ==
  | PLUS; { Ast.Pos }
  | MINUS; { Ast.Neg }

let bin_arit_op ==
  | PLUS; { Ast.Add }
  | MINUS; { Ast.Sub }
  | MULT; { Ast.Mul }
  | DIV; { Ast.Div }
  | MOD; { Ast.Mod }

let un_logic_op ==
  | NOT; { Ast.Not }

let bin_logic_op ==
  | AND; { Ast.And }
  | OR; { Ast.Or }

let comp_op ==
  | EQ; { Ast.Eq }
  | NOT_EQ; { Ast.Neq }
  | GREATER; { Ast.Gt }
  | LESSER; { Ast.Lt }
  | GREATER_EQ; { Ast.Geq }
  | LESSER_EQ; { Ast.Leq }

let var_type :=
  | dt = compl_data_type; dims = list(delimited(LEFT_BRACKET, LIT_INT, RIGHT_BRACKET));
    { Ast.create_var_type dt dims }

let param_type :=
  | dt = compl_data_type; dims = list(delimited(LEFT_BRACKET, LIT_INT, RIGHT_BRACKET));
    { Ast.create_param_type dt dims false }
  | dt = compl_data_type; LEFT_BRACKET; RIGHT_BRACKET; dims = list(delimited(LEFT_BRACKET, LIT_INT, RIGHT_BRACKET));
    { Ast.create_param_type dt dims true }

let ret_type :=
  | dt = incompl_data_type; { dt }
  | dt = compl_data_type; { dt }

let pass_by :=
  | { Ast.Value }
  | REF; { Ast.Reference }

let var_def :=
  | VAR; ids = separated_nonempty_list(COMMA, ID); COLON; vt = var_type; SEMICOLON;
    { List.map (fun id -> wrap_var_def $loc id vt tbl) ids }

let param_def :=
  | pb = pass_by; ids = separated_nonempty_list(COMMA, ID); COLON; pt = param_type;
    { List.map (fun id -> wrap_param_def $loc id pt pb tbl) ids }

let l_value :=
  | id = ID; exprs = list(delimited(LEFT_BRACKET, expr, RIGHT_BRACKET));
    { wrap_l_value_id $loc id exprs tbl }
  | str = LIT_STR; exprs = list(delimited(LEFT_BRACKET, expr, RIGHT_BRACKET));
    { wrap_l_value_string $loc str exprs tbl }

let func_call :=
  | id = ID; LEFT_PAR; exprs = separated_list(COMMA, expr); RIGHT_PAR;
    { wrap_func_call $loc id exprs tbl }

let expr :=
  | li = LIT_INT;
    { wrap_expr_lit_int $loc li tbl }
  | lc = LIT_CHAR;
    { wrap_expr_lit_char $loc lc tbl }
  | lv = l_value;
    { wrap_expr_l_value $loc lv tbl }
  | LEFT_PAR; ~ = expr; RIGHT_PAR;
    { expr }
  | fc = func_call;
    { wrap_expr_func_call $loc fc tbl }
  | op = un_arit_op; e = expr; %prec USIGN
    { wrap_expr_un_arit_op $loc op e tbl }  (* Semantic: e should be int *)
  | e1 = expr;  op = bin_arit_op; e2 = expr;
    { wrap_expr_bin_arit_op $loc op e1 e2 tbl } (* Semantic: e1, e2 should be both int *)

let cond :=
  | LEFT_PAR; c = cond; RIGHT_PAR;
    { c }
  | op = un_logic_op; c = cond; %prec UNOT
    { wrap_cond_un_logic_op $loc op c tbl }
  | c1 = cond; op = bin_logic_op; c2 = cond;
    { wrap_cond_bin_logic_op $loc op c1 c2 tbl } (* AND/OR should be short-circuited, added after semantic analysis? *)
  | e1 = expr; op = comp_op ; e2 = expr; (* %prec UCOMP *)
    { wrap_cond_comp_op $loc op e1 e2 tbl } (* Semantic: e{1,2} should be both int or both char *)

let block :=
  | LEFT_CURL; stmts = list(stmt); RIGHT_CURL;
    { wrap_block $loc stmts }

let stmt :=
  | SEMICOLON;
    { wrap_stmt_empty $loc tbl }
  | lv = l_value; ASSIGN; e = expr; SEMICOLON;
    { wrap_stmt_assign $loc lv e tbl }
  | b = block;
    { wrap_stmt_block $loc b tbl }
  | fc = func_call; SEMICOLON;
    { wrap_stmt_func_call $loc fc tbl }
  | IF; c = cond; THEN; s = stmt;
    { wrap_stmt_if $loc c (Some s) None tbl }
  | IF; c = cond; THEN; s1 = stmt; ELSE; s2 = stmt;
    { wrap_stmt_if $loc c (Some s1) (Some s2) tbl }
  | WHILE; c = cond; DO; s = stmt;
    { wrap_stmt_while $loc c s tbl }
  | RETURN; e_o = option(expr); SEMICOLON;
    { wrap_stmt_return $loc e_o tbl }

let decl_header :=
  | FUN; id = ID; LEFT_PAR; fd_l = flatten(separated_list(SEMICOLON, param_def)); RIGHT_PAR; COLON; rt = ret_type;
    { wrap_decl_header $loc(id) id fd_l rt tbl }

let def_header :=
  | FUN; id = ID; LEFT_PAR; fd_l = flatten(separated_list(SEMICOLON, param_def)); RIGHT_PAR; COLON; rt = ret_type;
    { wrap_def_header $loc(id) id fd_l rt tbl }

let func_decl :=
  | h = decl_header; SEMICOLON;
    { wrap_func_decl h }

let func_def :=
  | h = def_header; ld_l = flatten(list(local_def)); b = block;
    { wrap_close_scope $loc tbl ;wrap_func_def h ld_l b }

let local_def :=
  | fd = func_def;
    { wrap_local_def_fdef fd }
  | fd = func_decl;
    { wrap_local_def_fdecl fd }
  | vd_l = var_def;
    { wrap_local_def_var vd_l }

let program :=
  | midrule({ wrap_open_scope "global" tbl }); fd = func_def; EOF;
    { wrap_close_scope $loc tbl; wrap_program fd tbl }
