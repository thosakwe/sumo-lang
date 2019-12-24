%token LBRACKET RBRACKET LCURLY RCURLY LPAREN RPAREN
%token ARROW COLON COMMA DOT EQUALS PIPE SEMI QUESTION
%token EXTERNAL FINAL FN RETURN THIS VAR

%token TIMES DIV MOD PLUS MINUS
%token TIMES_EQUALS DIV_EQUALS MOD_EQUALS PLUS_EQUALS MINUS_EQUALS

%token <Visibility.t> VIS
%token <string> C_NAME

%token <int> INT
%token <float> DOUBLE
%token <bool> BOOL
%token <string> LOWER_ID UPPER_ID

%token EOF

%left TIMES DIV MOD
%left PLUS MINUS
%left TIMES_EQUALS DIV_EQUALS MOD_EQUALS
%left PLUS_EQUALS MINUS_EQUALS

%start <Ast.compilation_unit> compilation_unit

%%

compilation_unit: decls = list(decl) EOF { decls }

decl:
  | v = vis f = func { Ast.FuncDecl ($loc, v, f) }

func:
  | FN name = id s = func_sig b = block
    {
      (* If the body is empty, add a "return" *)
      let nonempty_body =
        match b with
        | [] -> [ Ast.Return ($loc(b), None) ]
        | _ -> b
      in
      Ast.ConcreteFunc ($loc, name, s, nonempty_body)
    }
  | EXTERNAL c = option(C_NAME) name = id s = func_sig
    {
      let cn =
        match c with
        | None -> None
        | Some str -> Some (String.sub str 1 ((String.length str) - 1))
      in
      Ast.ExternalFunc ($loc, cn, name, s)
    }


vis:
  | { Visibility.Private }
  | v = VIS { v }


func_sig:
  LPAREN
  params = separated_list(COMMA, param)
  RPAREN
  COLON
  t = typ
  {
    ($loc, params, t)
  }

param:
  | n = id COLON t = typ { Ast.RegularParam ($loc, n, t) }
  | THIS DOT n = id { Ast.ThisParam ($loc, n) }


typ:
  | v = id { Ast.TypeRef ($loc, v) }
  /*
  | v = typ QUESTION
    {  
      match v with
        | Ast.OptionalType (_, _) as self -> self
        | _  -> Ast.OptionalType ($loc, v)
    }
  */

block:
  | LCURLY s = list(stmt) RCURLY { s }
  | ARROW e = expr { [Ast.Return ($loc(e), (Some e))] }

stmt:
  | b = block { Ast.Block ($loc, b) }
  | e = expr { Ast.Expr ($loc, e) }
  | RETURN v = option(expr) { Ast.Return ($loc, v) }
  | m = final d = separated_list(COMMA, var_decl)
    {
      let expand_decl (span, t, name, value) =
        (span, m, t, name, value)
      in
      Ast.VarDecl (List.map expand_decl d)
    }


assign_target:
  | n = id { Ast.VariableTarget ($loc, n) }

var_decl:
  n = id t = option(var_type) EQUALS v = expr { ($loc, t, n, v) }

var_type: COLON t = typ { t }

final:
  | FINAL { true }
  | VAR { false }


expr:
  | v = INT { Ast.IntLiteral ($loc, v) }
  | v = DOUBLE { Ast.DoubleLiteral ($loc, v) }
  | v = BOOL { Ast.BoolLiteral ($loc, v) }
  | v = id { Ast.Ref ($loc, v) }
  | LPAREN v = expr RPAREN { Ast.Paren ($loc, v) }
  | t = expr LPAREN a = separated_list(COMMA, expr) RPAREN { Ast.Call ($loc, t, a) }
  | l = expr TIMES r = expr { Ast.Binary ($loc, l, Ast.Multiply, r) }
  | l = expr MOD r = expr { Ast.Binary ($loc, l, Ast.Modulo, r) }
  | l = expr DIV r = expr { Ast.Binary ($loc, l, Ast.Divide, r) }
  | l = expr PLUS r = expr { Ast.Binary ($loc, l, Ast.Plus, r) }
  | l = expr MINUS r = expr { Ast.Binary ($loc, l, Ast.Minus, r) }
  | t = assign_target EQUALS v = expr { Ast.Assign ($loc, t, Ast.Equals, v) }
  | t = assign_target TIMES_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.Multiply), v) }
  | t = assign_target DIV_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.Divide), v) }
  | t = assign_target MOD_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.Modulo), v) }
  | t = assign_target PLUS_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.Plus), v) }
  | t = assign_target MINUS_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.Minus), v) }


id:
  | v = UPPER_ID { v }
  | v = LOWER_ID { v }
