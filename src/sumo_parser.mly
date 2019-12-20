%token LBRACKET RBRACKET LCURLY RCURLY LPAREN RPAREN
%token ARROW COLON COMMA DOT EQUALS PIPE SEMI QUESTION

%token EXTERNAL FINAL FN RETURN THIS VAR
%token <Visibility.t> VIS
%token <string> C_NAME

%token <int> INT
%token <float> DOUBLE
%token <bool> BOOL
%token <string> LOWER_ID UPPER_ID

%token EOF

%start <Ast.compilation_unit> compilation_unit

%%

compilation_unit: decls = list(decl); EOF { decls }

decl:
  | v = vis; f = func { Ast.FuncDecl ($loc, v, f) }

func:
  | FN; name = id; s = func_sig; b = block
    {
      Ast.ConcreteFunc ($loc, name, s, b)
    }
  | EXTERNAL; c = option(C_NAME); name = id; s = func_sig
    {
      Ast.ExternalFunc ($loc, c, name, s)
    }
;

vis:
  | { Visibility.Private }
  | v = VIS { v }
;

func_sig:
  LPAREN;
  params = separated_list(COMMA, param);
  RPAREN;
  COLON;
  t = typ;
  {
    ($loc, params, t)
  }

param:
  | n = id; COLON; t = typ { Ast.RegularParam ($loc, n, t) }
  | THIS; DOT; n = id { Ast.ThisParam ($loc, n) }
;

typ:
  | v = id { Ast.TypeRef ($loc, v) }
  | v = typ; QUESTION
    {  
      match v with
        | Ast.OptionalType (_, _) as self -> self
        | _  -> Ast.OptionalType ($loc, v)
    }

block:
  | LCURLY; s = list(stmt); RCURLY { s }
  | ARROW; e = expr { [Ast.Return ($loc(e), (Some e))] }

stmt:
  | b = block { Ast.Block ($loc, b) }
  | e = expr { Ast.Expr ($loc, e) }
  | RETURN; v = option(expr) { Ast.Return ($loc, v) }
  | m = mut; d = separated_list(COMMA, var_decl)
    {
      let expand_decl (span, name, value) =
        (span, m, name, value)
      in
      Ast.VarDecl (List.map expand_decl d)
    }
;

var_decl:
  n = id; EQUALS; v = expr { ($loc, n, v) }
;

mut:
  | VAR { true }
  | FINAL { false }
;

expr:
  | v = INT { Ast.IntLiteral ($loc, v) }
  | v = DOUBLE { Ast.DoubleLiteral ($loc, v) }
  | v = BOOL { Ast.BoolLiteral ($loc, v) }
  | v = id { Ast.Ref ($loc, v) }
  | LPAREN; v = expr; RPAREN { Ast.Paren ($loc, v) }
;

id:
  | v = UPPER_ID { v }
  | v = LOWER_ID { v }
;