%{
  open Ast
  open Gift
%}

%left OR
%left AND
%nonassoc UNOT
// %nonassoc UCOMP
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc USIGN

%nonassoc THEN
%nonassoc ELSE

(* --- some ideas for what the final types will look like? --- *)
%start <Ast.func_def Ast.node> program

%type <Ast.func_def Ast.node> func_def
%type <Ast.header Ast.node> def_header
%type <Ast.header Ast.node> decl_header
%type <Ast.param Ast.node list> fpar_def
%type <Ast.data> data_type
%type <Ast.data> var_type
%type <Ast.data> ret_type
%type <Ast.data> fpar_type
%type <Ast.local_def Ast.node list> local_def
%type <Ast.func_decl Ast.node> func_decl
%type <Ast.var Ast.node list> var_def
%type <Ast.stmt Ast.node> stmt
%type <Ast.block Ast.node> block
%type <Ast.func_call Ast.node> func_call
%type <Ast.l_value Ast.node> l_value
%type <Ast.expr Ast.node> expr
%type <Ast.uarit> sign_op
%type <Ast.arit> arit_op
%type <Ast.cond Ast.node> cond
%type <Ast.logic> logic_op
%type <Ast.comp> comp_op

%%

let program :=
  | midrule({ wrap_open_scope tbl }); fd = func_def; EOF;
    { wrap_close_scope $loc tbl; fd }

let func_def :=
  | h = def_header; ld = flatten(list(local_def)); b = block; 
    { wrap_close_scope $loc tbl ;wrap_func_def $loc(h) (h, ld, b) }

let def_header := 
  | FUN; id = ID; LEFT_PAR; fd = flatten(separated_list(SEMICOLON, fpar_def)); RIGHT_PAR; COLON; rt = ret_type;   
    { wrap_def_header $loc(id) (id, fd, rt) tbl }

let fpar_def :=
  | ~ = pass; ids = separated_nonempty_list(COMMA, ID); COLON; dt = fpar_type;
    { List.map (fun id -> wrap_param $loc (id, dt, pass) tbl) ids }

let var_type :=
  | dt = data_type; dims = list(delimited(LEFT_BRACKET, LIT_INT, RIGHT_BRACKET));
    { Ast.get_var dt dims }

let ret_type :=
  | NOTHING; { Nothing }
  | ~ = data_type; { data_type }

let fpar_type :=
  | dt = data_type; dims = list(delimited(LEFT_BRACKET, LIT_INT, RIGHT_BRACKET));
    { Ast.get_param dt dims false }
      
  | dt = data_type; LEFT_BRACKET; RIGHT_BRACKET; dims = list(delimited(LEFT_BRACKET, LIT_INT, RIGHT_BRACKET));
    { Ast.get_param dt dims true }

let local_def :=   
  | fd = func_def;
    { wrap_local_def $loc (`FuncDef fd) }
  | fd = func_decl;
    { wrap_local_def $loc (`FuncDecl fd) }
  | vd = var_def;
    { wrap_local_def $loc (`VarDefList vd) }

let func_decl :=   
  | h = decl_header; SEMICOLON;
    { wrap_func_decl $loc(h) h }

let decl_header :=
    | FUN; id = ID; LEFT_PAR; fd = flatten(separated_list(SEMICOLON, fpar_def)); RIGHT_PAR; COLON; rt = ret_type;
    { wrap_decl_header $loc(id) (id, fd, rt) tbl }

let var_def :=
  | VAR; ids = separated_nonempty_list(COMMA, ID); COLON; dt = var_type; SEMICOLON;                   
    { List.map (fun id -> wrap_var $loc (id, dt) tbl) ids }

let stmt :=   
  | SEMICOLON;                                             
    { wrap_stmt $loc Ast.Empty tbl }
  | lv = l_value; ASSIGN; e = expr; SEMICOLON;                           
    { wrap_stmt $loc (Ast.Assign (lv, e)) tbl }
  | ~ = block;                                           
    { wrap_stmt $loc (Ast.Block block) tbl }
  | ~ = func_call; SEMICOLON;                                   
    { wrap_stmt $loc (Ast.SFuncCall func_call) tbl }
  | IF; ~ = cond; THEN; ~ = stmt;                               
    { wrap_stmt $loc (Ast.If (cond, Some stmt, None)) tbl }
  | IF; ~ = cond; THEN; stmt1 = stmt; ELSE; stmt2 = stmt;                     
    { wrap_stmt $loc (Ast.If (cond, Some stmt1, Some stmt2)) tbl }
  | WHILE; ~ = cond; DO; ~ = stmt;                        
    { wrap_stmt $loc (Ast.While (cond, stmt)) tbl }
  | RETURN; expr_o = option(expr); SEMICOLON;                              
    { wrap_stmt $loc (Ast.Return expr_o) tbl }

let block :=   
  | LEFT_CURL; stmt_list = list(stmt); RIGHT_CURL;                                    
    { wrap_block $loc stmt_list }

let func_call :=   
  | id = ID; LEFT_PAR; e_l = separated_list(COMMA, expr); RIGHT_PAR;                     
    { wrap_func_call $loc (id, e_l) tbl }

let l_value :=   
  | id = ID;                                              
    { wrap_l_value $loc (Ast.Id id) tbl }
  | str = LIT_STR;                                         
    { wrap_l_value $loc (Ast.LString str) tbl }
  | lv = l_value; LEFT_BRACKET; e = expr; RIGHT_BRACKET;                            
    { wrap_l_value $loc (Ast.ArrayAccess (lv, e)) tbl }

let expr :=   
  | li = LIT_INT;                                         
    { wrap_expr $loc (Ast.LitInt li) tbl }
  | lc = LIT_CHAR;                                        
    { wrap_expr $loc (Ast.LitChar lc) tbl }
  | lv = l_value;                                         
    { wrap_expr $loc (Ast.LValue lv) tbl }
  | LEFT_PAR; ~ = expr; RIGHT_PAR;                                    
    { expr }
  | fc = func_call;                                       
    { wrap_expr $loc (Ast.EFuncCall fc) tbl }
  | op = sign_op; e = expr; %prec USIGN
    { wrap_expr $loc (Ast.Signed (op, e)) tbl }  (* Semantic: expr should be int *)
  | e1 = expr;  op = arit_op; e2 = expr;                            
    { wrap_expr $loc (Ast.AritOp (e1, op, e2)) tbl } (* Semantic: e{1,2} should be both int *)

let cond :=
  | LEFT_PAR; ~ = cond; RIGHT_PAR;                                    
    { cond }
  | NOT; ~ = cond; %prec UNOT                                       
    { wrap_cond $loc (Ast.Not cond) tbl }
  | c1 = cond; op = logic_op; c2 = cond;                                  
    { wrap_cond $loc (Ast.Logic (c1, op, c2)) tbl } (* AND/OR should be short-circuited, added after semantic analysis? *)
  | e1 = expr; op = comp_op ; e2 = expr; (* %prec UCOMP *)   
    { wrap_cond $loc (Ast.Comp (e1, op, e2)) tbl } (* Semantic: e{1,2} should be both int or both char *)

let data_type :=
  | INT; { Int }
  | CHAR; { Char }

let pass :=
  | { ByValue }
  | REF; { ByReference }

let sign_op ==
  | PLUS; { Pos }
  | MINUS; { Neg }

let arit_op ==
  | PLUS; { Add }
  | MINUS; { Sub }
  | MULT; { Mul }
  | DIV; { Div }
  | MOD; { Mod }

let logic_op ==
  | AND; { And }
  | OR; { Or }

let comp_op ==
  | EQ; { Eq }
  | NOT_EQ; { Neq }
  | LESS; { Lt }
  | MORE; { Gt }
  | LESS_EQ; { Leq }
  | MORE_EQ; { Geq }
