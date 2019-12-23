%token LBRACKET RBRACKET LCURLY RCURLY LPAREN RPAREN
%token ARROW COLON COMMA DOT EQUALS PIPE SEMI QUESTION
%token EXTERNAL FINAL FN RETURN THIS VAR

%token <Ast.assign_op> ASSIGN_OP
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
      (* If the body is empty, add a "return" *)
      let nonempty_body =
        match b with
        | [] -> [ Ast.Return ($loc(b), None) ]
        | _ -> b
      in
      Ast.ConcreteFunc ($loc, name, s, nonempty_body)
    }
  | EXTERNAL; c = option(C_NAME); name = id; s = func_sig
    {
      let cn =
        match c with
        | None -> None
        | Some str -> Some (String.sub str 1 ((String.length str) - 1))
      in
      Ast.ExternalFunc ($loc, cn, name, s)
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
  /*
  | v = typ; QUESTION
    {  
      match v with
        | Ast.OptionalType (_, _) as self -> self
        | _  -> Ast.OptionalType ($loc, v)
    }
  */

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
  | t = assign_target; o = assign_op; v = expr { ($loc, t, o, v) }
;

assign_target:
  | n = id { Ast.VariableTarget ($loc, n) };

assign_op:
  | v = ASSIGN_OP { v }
  | EQUALS { Ast.Equals };

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
  | t = expr; LPAREN; a = separated_list(COMMA, expr); RPAREN { Ast.Call ($loc, t, a) }
;

id:
  | v = UPPER_ID { v }
  | v = LOWER_ID { v }
;