(* varible identifiers *)
%token <string> ID

(* literals *)
%token <int> LIT_INT
%token <char> LIT_CHAR
%token <string> LIT_STR

(* reserved type names *)
%token CHAR
%token INT
%token NOTHING

(* statement keywords *)
%token VAR
%token FUN
%token REF
%token RETURN
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO

(* relational operators *)
%token EQ
%token NOT_EQ
%token GREATER
%token LESSER
%token GREATER_EQ
%token LESSER_EQ

(* logical operators *)
%token AND
%token OR
%token NOT

(* arithmetic operators *)
%token PLUS
%token MINUS
%token MULT
%token DIV
%token MOD

(* structural symbols *)
%token LEFT_PAR
%token RIGHT_PAR
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_CURL
%token RIGHT_CURL
%token COMMA
%token COLON
%token SEMICOLON
%token ASSIGN

(* end of file *)
%token EOF

%left OR
%left AND
%nonassoc UNOT
// %nonassoc UCOMP
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc USIGN

%nonassoc THEN
%nonassoc ELSE

%%
