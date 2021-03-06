%token LBRACKET RBRACKET LCURLY RCURLY LPAREN RPAREN
%token ARROW COLON COMMA DOT EQUALS SEMI QUESTION
%token DO ELSE EXTERNAL FINAL FN FOR HIDE IF IMPORT RETURN
%token SHOW THIS TYPE VAR WHILE MATCH WITH AS

%token TIMES DIV MOD PLUS MINUS SHL SHR LT LTE GT GTE BOOL_EQ BOOL_NEQ
%token BW_AND BW_XOR BW_OR BOOL_AND BOOL_OR INCR DECR BOOL_NOT BW_NOT
%token TIMES_EQUALS DIV_EQUALS MOD_EQUALS PLUS_EQUALS MINUS_EQUALS
%token SHL_EQUALS SHR_EQUALS BW_AND_EQUALS BW_XOR_EQUALS BW_OR_EQUALS
%token BOOL_AND_EQUALS BOOL_OR_EQUALS

%token <Visibility.t> VIS
%token <string> C_NAME

%token <int> INT
%token <float> DOUBLE
%token <bool> BOOL
%token NONE
%token <string> STRING
%token <string> LOWER_ID UPPER_ID

%token EOF

%left BOOL_AND_EQUALS BOOL_OR_EQUALS
%left BW_OR_EQUALS
%left BW_XOR_EQUALS
%left BW_AND_EQUALS
%left SHL_EQUALS SHR_EQUALS
%left PLUS_EQUALS MINUS_EQUALS
%left TIMES_EQUALS DIV_EQUALS MOD_EQUALS
%left BOOL_AND BOOL_OR
%left BW_OR
%left BW_XOR
%left BW_AND
%left BOOL_EQ BOOL_NEQ
%left GT GTE
%left LT LTE
%left SHL SHR
%left PLUS MINUS
%left TIMES DIV MOD
%left BOOL_NOT BW_NOT
%left INCR DECR
%left LPAREN

%start <Ast.compilation_unit> compilation_unit

%%

compilation_unit: dirs = list(directive) decls = list(decl) EOF { (dirs, decls) }

directive:
  | IMPORT s = STRING m = option(import_modifier) { Ast.ImportDirective ($loc, s, m) }

import_modifier:
  | SHOW n = separated_list(COMMA, spanned_id) { Ast.Show ($loc, n) }
  | HIDE n = separated_list(COMMA, spanned_id) { Ast.Hide ($loc, n) }

decl:
  | v = vis f = func { Ast.FuncDecl ($loc, v, f) }
  | v = vis TYPE n = id EQUALS t = typ { Ast.TypeDecl ($loc, v, n, t) }

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
  | VAR v = variant_list { Ast.VariantType ($loc, v) }
  | v = id { Ast.TypeRef ($loc, v) }
  | v = typ QUESTION
    {  
      match v with
        | Ast.OptionalType (_, _) as self -> self
        | _  -> Ast.OptionalType ($loc, v)
    }
  | LCURLY f = list(struct_type_field) RCURLY { Ast.StructType ($loc, f) }

variant_list: option(BW_OR) v = separated_list(BW_OR, variant) { v }

variant: 
  | n = id { ($loc, n, []) }
  | n = id LPAREN t = separated_list(COMMA, typ) RPAREN { ($loc, n, t) }

struct_type_field: n = id COLON t = typ { ($loc, n, t) }

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
      Ast.VarDecl ($loc, (List.map expand_decl d))
    }
  /* | i = if_clause ei = list(else_if_clause) e = option(else_clause) */
  | i = if_clause e = option(else_clause)
    { Ast.If ($loc, i, [], e) }
  | WHILE c = expr b = stmt { Ast.While ($loc, c, b) }
  | DO b = stmt WHILE c = expr { Ast.DoWhile ($loc, b, c) }
  | FOR LPAREN i = option(stmt) SEMI c = expr SEMI p = separated_list(SEMI, stmt) RPAREN b = stmt
    { Ast.ForLoop ($loc, i, c, p, b) }
  | MATCH cond = expr WITH LCURLY clauses = list(match_clause) RCURLY
    { Ast.MatchStmt ($loc, cond, clauses) }

match_clause: p = pattern b = block { ($loc, p, b) }

pattern:
  | n = id LPAREN RPAREN { Ast.ConstructorPattern ($loc, n, []) }
  | n = id LPAREN p = separated_list(COMMA, pattern) RPAREN { Ast.ConstructorPattern ($loc, n, p) }
  | n = id { if n = "_" then (Ast.IgnoredPattern $loc) else (Ast.NamedPattern ($loc, n)) }
  | LCURLY p = separated_list(COMMA, struct_pattern) RCURLY { Ast.StructPattern ($loc, p) }
  | p = pattern AS n = id { Ast.AliasedPattern($loc, p, n) }
  | option(BW_OR) p = separated_list(BW_OR, pattern) { Ast.MultiPattern($loc, p) }
  | EQUALS v = expr { Ast.ExprPattern ($loc, v) }
  | LPAREN p = pattern RPAREN { p }

struct_pattern:
  | n = id { ($loc, n, (if n = "_" then (Ast.IgnoredPattern $loc) else (Ast.NamedPattern ($loc, n)))) }
  | n = id COLON p = pattern { ($loc, n, p) }

if_clause:
  | IF LPAREN c = expr RPAREN b = stmt { Ast.BasicIfClause ($loc, c, b) }
  | IF LPAREN m = final d = separated_list(COMMA, var_decl) RPAREN b = stmt
    {
      let expand_decl (span, t, name, value) =
        (span, m, t, name, value)
      in
      Ast.NullCheckIfClause ($loc, (List.map expand_decl d), b)
    }


else_clause: ELSE b = stmt { b }

/* else_if_clause: ELIF v = if_clause { v } */

assign_target:
  | n = id { Ast.VariableTarget ($loc, n) }
  | v = expr DOT n = id { Ast.FieldTarget ($loc, v, n) }

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
  | NONE { Ast.NoneLiteral $loc }
  | LPAREN v = expr RPAREN { Ast.Paren ($loc, v) }
  | LCURLY f = separated_list(option(COMMA), struct_value_field) RCURLY { Ast.StructLiteral ($loc, f) }
  | v = expr INCR { Ast.Unary ($loc, v, Ast.PostfixIncrement) }
  | v = expr DECR { Ast.Unary ($loc, v, Ast.PostfixDecrement) }
  | t = expr LPAREN a = separated_list(COMMA, expr) RPAREN { Ast.Call ($loc, t, a) }
  | v = expr DOT n = id { Ast.GetField ($loc, v, n) }
  | INCR v = expr { Ast.Unary ($loc, v, Ast.PrefixIncrement) }
  | DECR v = expr { Ast.Unary ($loc, v, Ast.PrefixDecrement) }
  | PLUS v = expr { Ast.Unary ($loc, v, Ast.UnaryPlus) }
  | MINUS v = expr { Ast.Unary ($loc, v, Ast.UnaryMinus) }
  | BOOL_NOT v = expr { Ast.Unary ($loc, v, Ast.LogicalNot) }
  | BW_NOT v = expr { Ast.Unary ($loc, v, Ast.BitwiseNot) }
  | l = expr TIMES r = expr { Ast.Binary ($loc, l, Ast.Multiply, r) }
  | l = expr MOD r = expr { Ast.Binary ($loc, l, Ast.Modulo, r) }
  | l = expr DIV r = expr { Ast.Binary ($loc, l, Ast.Divide, r) }
  | l = expr PLUS r = expr { Ast.Binary ($loc, l, Ast.Plus, r) }
  | l = expr MINUS r = expr { Ast.Binary ($loc, l, Ast.Minus, r) }
  | l = expr SHL r = expr { Ast.Binary ($loc, l, (Ast.Shift Ast.Left), r) }
  | l = expr SHR r = expr { Ast.Binary ($loc, l, (Ast.Shift Ast.Right), r) }
  | l = expr LT r = expr { Ast.Binary ($loc, l, Ast.Lt, r) }
  | l = expr LTE r = expr { Ast.Binary ($loc, l, Ast.Lte, r) }
  | l = expr GT r = expr { Ast.Binary ($loc, l, Ast.Gt, r) }
  | l = expr GTE r = expr { Ast.Binary ($loc, l, Ast.Gte, r) }
  | l = expr BOOL_EQ r = expr { Ast.Binary ($loc, l, Ast.Eq, r) }
  | l = expr BOOL_NEQ r = expr { Ast.Binary ($loc, l, Ast.Neq, r) }
  | t = assign_target EQUALS v = expr { Ast.Assign ($loc, t, Ast.Equals, v) }
  | l = expr BW_AND r = expr { Ast.Binary ($loc, l, (Ast.Bitwise Ast.BitwiseAnd), r) }
  | l = expr BW_XOR r = expr { Ast.Binary ($loc, l, (Ast.Bitwise Ast.BitwiseXor), r) }
  | l = expr BW_OR r = expr { Ast.Binary ($loc, l, (Ast.Bitwise Ast.BitwiseOr), r) }
  | l = expr BOOL_AND r = expr { Ast.Binary ($loc, l, Ast.BooleanAnd, r) }
  | l = expr BOOL_OR r = expr { Ast.Binary ($loc, l, Ast.BooleanOr, r) }
  | t = assign_target TIMES_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.Multiply), v) }
  | t = assign_target DIV_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.Divide), v) }
  | t = assign_target MOD_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.Modulo), v) }
  | t = assign_target PLUS_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.Plus), v) }
  | t = assign_target MINUS_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.Minus), v) }
  | t = assign_target SHL_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign (Ast.Shift Ast.Left)), v) }
  | t = assign_target SHR_EQUALS v = expr { Ast.Assign ($loc, t, (Ast.BinaryAssign (Ast.Shift Ast.Right)), v) }
  | t = assign_target BW_AND_EQUALS v = expr
    { Ast.Assign ($loc, t, (Ast.BinaryAssign (Ast.Bitwise Ast.BitwiseAnd)), v) }
  | t = assign_target BW_XOR_EQUALS v = expr
    { Ast.Assign ($loc, t, (Ast.BinaryAssign (Ast.Bitwise Ast.BitwiseXor)), v) }
  | t = assign_target BW_OR_EQUALS v = expr
    { Ast.Assign ($loc, t, (Ast.BinaryAssign (Ast.Bitwise Ast.BitwiseOr)), v) }
  | t = assign_target BOOL_AND_EQUALS v = expr
    { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.BooleanAnd), v) }
  | t = assign_target BOOL_OR_EQUALS v = expr
    { Ast.Assign ($loc, t, (Ast.BinaryAssign Ast.BooleanOr), v) }


struct_value_field: n = id COLON v = expr { ($loc, n, v) }

id:
  | v = UPPER_ID { v }
  | v = LOWER_ID { v }

spanned_id: v = id { ($loc, v) }
