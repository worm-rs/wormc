(* defines parser structure *)
type parser = {
  (* source used for error reporting *)
  source: Grace.Source.t ref;

  (* lexer used for tokens iteration *)
  lx: Lex.lexer;

  (* current token *)
  mutable current: Token.t option;

  (* next token *)
  mutable next: Token.t option
}

(* creates new parser *)
let create (lx : Lex.lexer) (source : Grace.Source.t ref) : parser = {
  lx; source; current = Lex.next_token lx; next = Lex.next_token lx
}

(* reports end of file *)
let eof () = Report.report
  (Report.diag
    ~notes:[]
    ~code: 4
    ~msg: (Grace.Diagnostic.Message.of_string "unexpected eof")
    ~severity: Grace.Diagnostic.Severity.Error
    ~labels: [])

(* reports unexpected token*)
let unexpected_tok (ps: parser) (span: Span.t) (expected: Token.kind) (got: Token.kind) = Report.report
  (Report.diag
    ~notes:[]
    ~code: 4
    ~msg: (Grace.Diagnostic.Message.of_string
      ("unexpected token. expected "
        ^ Token.string_of_kind expected
        ^ ", got: "
        ^ Token.string_of_kind got))
    ~severity: Grace.Diagnostic.Severity.Error
    ~labels: [
      Grace.Diagnostic.Label.create
        ~range: (Report.span_to_range
          ps.source
          span
        )
        ~priority: Grace.Diagnostic.Priority.Primary
        (Grace.Diagnostic.Message.of_string "here")
    ])

(* reports unexpected expr token*)
let unexpected_expr_tok (ps: parser) (span: Span.t) = Report.report
  (Report.diag
    ~notes: [Grace.Diagnostic.Message.of_string "this token could not be start of the expression"]
    ~code: 4
    ~msg: (Grace.Diagnostic.Message.of_string "unexpected token")
    ~severity: Grace.Diagnostic.Severity.Error
    ~labels: [
      Grace.Diagnostic.Label.create
        ~range: (Report.span_to_range
          ps.source
          span
        )
        ~priority: Grace.Diagnostic.Priority.Primary
        (Grace.Diagnostic.Message.of_string "here")
    ])

(* expectes token *)
let expect (ps: parser) (kind: Token.kind) =
  match ps.current with
    | Some tok when tok.kind = kind ->
      ps.current <- ps.next;
      ps.next <- Lex.next_token ps.lx
    | Some tok -> unexpected_tok ps tok.span kind tok.kind
    | None -> eof ()

(* bump token *)
let bump (ps: parser) =
  ps.current <- ps.next;
  ps.next <- Lex.next_token ps.lx

(* checks token match *)
let check (ps: parser) (kind: Token.kind) : bool =
  match ps.current with
  | Some tok when tok.kind = kind -> true
  | Some _ -> false
  | None -> eof ()

(* peeks token *)
let peek (ps: parser) : Token.t =
  match ps.current with
  | Some tok -> tok
  | None -> eof ()

(* parses atom expression *)
let atom_expr (ps: parser) : Ast.expr =
  match ps.current with
  | Some tok ->
      (match tok.kind with
      | Token.Int i -> { span = tok.span; kind = Ast.Lit (Ast.Int i) }
      | _ -> unexpected_expr_tok ps tok.span)
  | None -> eof ()

(*parses unary expression *)
let unary_expr (ps: parser) : Ast.expr =
  if (check ps Token.Minus) || (check ps Token.Bang) then
    (* parsing unary operator *)
    let op_tok = (peek ps) in
    let op = match op_tok.kind with
    | Token.Minus -> Ast.Neg
    | Token.Bang -> Ast.Not
    | _ -> failwith "unreachable" in
    bump ps;

    (* parsing atom expression *)
    let expr = atom_expr ps in

    (* forming expression *)
    {
      span = { start_pos = op_tok.span.start_pos; end_pos = expr.span.end_pos };
      kind = Ast.Unary (op, expr)
    }
  else
  atom_expr ps

(*parses factor expression *)
let factor_expr (ps: parser) : Ast.expr =
  (* parsing left expression *)
  let start = peek ps in
  let left = ref (unary_expr ps) in

  (* parsing suffix *)
  while (check ps Token.Star) || (check ps Token.Slash) || (check ps Token.Percent) do
    (* parsing binary operator *)
    let op_tok = (peek ps) in
    let op = match op_tok.kind with
    | Token.Star -> Ast.Mul
    | Token.Slash -> Ast.Div
    | Token.Percent -> Ast.Mod
    | _ -> failwith "unreachable" in
    bump ps;

    (* parsing right expression *)
    let right = unary_expr ps in

    (* forming expression *)
    left := {
      span = { start_pos = start.span.start_pos; end_pos = right.span.end_pos };
      kind = Ast.Bin (op, !left, right)
    }
  done;

  !left

(*parses term expression *)
let term_expr (ps: parser) : Ast.expr =
  (* parsing left expression *)
  let start = peek ps in
  let left = ref (factor_expr ps) in

  (* parsing suffix *)
  while (check ps Token.Plus) || (check ps Token.Minus) do
    (* parsing binary operator *)
    let op_tok = (peek ps) in
    let op = match op_tok.kind with
    | Token.Plus -> Ast.Add
    | Token.Minus -> Ast.Sub
    | _ -> failwith "unreachable" in
    bump ps;

    (* parsing right expression *)
    let right = factor_expr ps in

    (* forming expression *)
    left := {
      span = { start_pos = start.span.start_pos; end_pos = right.span.end_pos };
      kind = Ast.Bin (op, !left, right)
    }
  done;

  !left

(*parses compare expression *)
let compare_expr (ps: parser) : Ast.expr =
  (* parsing left expression *)
  let start = peek ps in
  let left = ref (term_expr ps) in

  (* parsing suffix *)
  while (check ps Token.Ge) || (check ps Token.Le) ||
        (check ps Token.Gt) || (check ps Token.Lt) do
    (* parsing binary operator *)
    let op_tok = (peek ps) in
    let op = match op_tok.kind with
    | Token.Ge -> Ast.Ge
    | Token.Gt -> Ast.Gt
    | Token.Le -> Ast.Le
    | Token.Lt -> Ast.Lt
    | _ -> failwith "unreachable" in
    bump ps;

    (* parsing right expression *)
    let right = term_expr ps in

    (* forming expression *)
    left := {
      span = { start_pos = start.span.start_pos; end_pos = right.span.end_pos };
      kind = Ast.Bin (op, !left, right)
    }
  done;

  !left

(*parses equality expression *)
let equality_expr (ps: parser) : Ast.expr =
  (* parsing left expression *)
  let start = peek ps in
  let left = ref (compare_expr ps) in

  (* parsing suffix *)
  while (check ps Token.Eq2) || (check ps Token.BangEq) do
    (* parsing binary operator *)
    let op_tok = (peek ps) in
    let op = match op_tok.kind with
    | Token.Eq2 -> Ast.Eq
    | Token.BangEq -> Ast.Ne
    | _ -> failwith "unreachable" in
    bump ps;

    (* parsing right expression *)
    let right = compare_expr ps in

    (* forming expression *)
    left := {
      span = { start_pos = start.span.start_pos; end_pos = right.span.end_pos };
      kind = Ast.Bin (op, !left, right)
    }
  done;

  !left

(*parses bitwise and expression *)
let bit_and_expr (ps: parser) : Ast.expr =
  (* parsing left expression *)
  let start = peek ps in
  let left = ref (equality_expr ps) in

  (* parsing suffix *)
  while (check ps Token.Amp) do
    (* parsing binary operator *)
    bump ps;

    (* parsing right expression *)
    let right = equality_expr ps in

    (* forming expression *)
    left := {
      span = { start_pos = start.span.start_pos; end_pos = right.span.end_pos };
      kind = Ast.Bin (Ast.BitAnd, !left, right)
    }
  done;

  !left

(*parses bitwise xor expression *)
let bit_xor_expr (ps: parser) : Ast.expr =
  (* parsing left expression *)
  let start = peek ps in
  let left = ref (bit_and_expr ps) in

  (* parsing suffix *)
  while (check ps Token.Caret) do
    (* parsing binary operator *)
    bump ps;

    (* parsing right expression *)
    let right = bit_and_expr ps in

    (* forming expression *)
    left := {
      span = { start_pos = start.span.start_pos; end_pos = right.span.end_pos };
      kind = Ast.Bin (Ast.Xor, !left, right)
    }
  done;

  !left

(*parses bitwise or expression *)
let bit_or_expr (ps: parser) : Ast.expr =
  (* parsing left expression *)
  let start = peek ps in
  let left = ref (bit_xor_expr ps) in

  (* parsing suffix *)
  while (check ps Token.Bar) do
    (* parsing binary operator *)
    bump ps;

    (* parsing right expression *)
    let right = bit_xor_expr ps in

    (* forming expression *)
    left := {
      span = { start_pos = start.span.start_pos; end_pos = right.span.end_pos };
      kind = Ast.Bin (Ast.BitOr, !left, right)
    }
  done;

  !left

(*parses logical and expression *)
let logical_and_expr (ps: parser) : Ast.expr =
  (* parsing left expression *)
  let start = peek ps in
  let left = ref (bit_or_expr ps) in

  (* parsing suffix *)
  while (check ps Token.Amp2) do
    (* parsing binary operator *)
    bump ps;

    (* parsing right expression *)
    let right = bit_or_expr ps in

    (* forming expression *)
    left := {
      span = { start_pos = start.span.start_pos; end_pos = right.span.end_pos };
      kind = Ast.Bin (Ast.And, !left, right)
    }
  done;

  !left

(*parses logical or expression *)
let logical_or_expr (ps: parser) : Ast.expr =
  (* parsing left expression *)
  let start = peek ps in
  let left = ref (logical_and_expr ps) in

  (* parsing suffix *)
  while (check ps Token.Amp2) do
    (* parsing binary operator *)
    bump ps;

    (* parsing right expression *)
    let right = logical_and_expr ps in

    (* forming expression *)
    left := {
      span = { start_pos = start.span.start_pos; end_pos = right.span.end_pos };
      kind = Ast.Bin (Ast.Or, !left, right)
    }
  done;

  !left

(*parses assign expression *)
let assign_expr (ps: parser) : Ast.expr =
  (* parsing left expression *)
  let start = peek ps in
  let left = ref (logical_or_expr ps) in

  (* parsing suffix *)
  while (check ps Token.Eq) do
    (* parsing binary operator *)
    bump ps;

    (* parsing right expression *)
    let right = logical_or_expr ps in

    (* forming expression *)
    left := {
      span = { start_pos = start.span.start_pos; end_pos = right.span.end_pos };
      kind = Ast.Assign (!left, right)
    }
  done;

  !left

(* parses expression *)
let expr (ps: parser) : Ast.expr =
  assign_expr ps
