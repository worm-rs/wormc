(* defines lexer structure *)
type lexer = {
  (* source code we lexing *)
  text: string;

  (* source used for error reporting *)
  source: Grace.Source.t ref;

  (* position *)
  mutable pos: int;
}

(* creates new lexer *)
let create (text : string) (source : Grace.Source.t ref) : lexer = { text; source; pos = 0 }

(* peeks current char *)
let peek (lx : lexer) : char option =
  if lx.pos >= String.length lx.text then None
  else Some(lx.text.[lx.pos])

(* peeks next char *)
let next (lx : lexer) : char option =
  if lx.pos + 1 >= String.length lx.text then None
  else Some(lx.text.[lx.pos + 1])

(* bumps char by adding 1 to pos *)
let bump (lx : lexer) =
  lx.pos <- lx.pos + 1

(* bumps chars by adding 2 to pos *)
let bump2 (lx : lexer) =
  lx.pos <- lx.pos + 2

(* checks current char is whitespace *)
let is_whitespace (lx : lexer) : bool =
  match peek lx with
  | Some '\n' | Some '\t' | Some ' ' -> true
  | _ -> false

(* checks current char is number *)
let is_number (lx : lexer) : bool =
  let ch = peek lx in
  match ch with
  | Some ch -> Char.Ascii.is_digit ch
  | None -> false

(* checks current char is number *)
let is_id (lx : lexer) : bool =
  let ch = peek lx in
  match ch with
  | Some ch -> Char.Ascii.is_letter ch || ch == '_'
  | None -> false

(* skips comment *)
let skip_comment (lx : lexer) =
  (* bumping `(` and `*` *)
  let start = lx.pos in
  bump2 lx;

  let done_ = ref false in
  while not !done_ do
    match peek lx, next lx with
    | Some '*', Some ')' ->
        bump2 lx; (* `*` and `)` *)
        done_ := true
    | None, _ ->
      Report.report
        (Report.diag
          ~notes:[]
          ~code: 1
          ~msg: (Grace.Diagnostic.Message.of_string "unterminated comment")
          ~severity: Grace.Diagnostic.Severity.Error
          ~labels: [
            Grace.Diagnostic.Label.create
              ~range: (Report.span_to_range
                lx.source
                (Span.create
                  ~start_pos: start
                  ~end_pos: lx.pos)
              )
              ~priority: Grace.Diagnostic.Priority.Primary
              (Grace.Diagnostic.Message.of_string "here")
          ])
    | _,  _ ->
        bump lx
  done

(* skips trivia chars *)
let skip_trivia (lx : lexer) =
  let is_done = ref false in
  while not !is_done do
    (* skipping whitespaces *)
    while is_whitespace lx do
      bump lx
    done;

    (* skipping comments *)
    match peek lx, next lx with
    | Some '(', Some '#' -> skip_comment lx
    | _ -> is_done := true
  done

(* converts id to token *)
let id_to_tk (kw: string) =
  match kw with
  | "let" -> Token.Let
  | "fn" -> Token.Fn
  | "if" -> Token.If
  | "else" -> Token.Else
  | "use" -> Token.Use
  | "struct" -> Token.Struct
  | "enum" -> Token.Enum
  | "type" -> Token.Type
  | "extern" -> Token.Extern
  | "for" -> Token.For
  | "as" -> Token.As
  | "true" -> Token.True
  | "false" -> Token.False
  | ident -> Token.Ident ident

(* parses string *)
let lex_string (lx: lexer) : Token.kind =
  (* skip opening quote *)
  let start = lx.pos in
  bump lx;

  let buf = Buffer.create 16 in
  let is_done = ref false in

  while not !is_done do
    match peek lx with
    | Some '"' ->
        bump lx; (* closing quote *)
        is_done := true
    | Some c ->
        Buffer.add_char buf c;
        bump lx
    | None ->
      Report.report
        (Report.diag
          ~notes:[]
          ~code: 2
          ~msg: (Grace.Diagnostic.Message.of_string "untermianted string")
          ~severity: Grace.Diagnostic.Severity.Error
          ~labels: [
            Grace.Diagnostic.Label.create
              ~range: (Report.span_to_range
                lx.source
                (Span.create
                  ~start_pos: start
                  ~end_pos: lx.pos)
              )
              ~priority: Grace.Diagnostic.Priority.Primary
              (Grace.Diagnostic.Message.of_string "here")
          ])
  done;

  Token.String (Buffer.contents buf)

(* parses number *)
let lex_number (lx: lexer) : Token.kind =
  let start = lx.pos in
  let buf = Buffer.create 8 in
  let is_done = ref false in
  let is_float = ref false in

  while not !is_done do
    match peek lx with
    | Some c when is_number lx ->
      Buffer.add_char buf c;
      bump lx;
    | Some '.' when not !is_float ->
      Buffer.add_char buf '.';
      bump lx;
      is_float := true;
    | Some '.' when !is_float ->
      Report.report
        (Report.diag
          ~notes:[]
          ~code: 1
          ~msg: (Grace.Diagnostic.Message.of_string "invalid float")
          ~severity: Grace.Diagnostic.Severity.Error
          ~labels: [
            Grace.Diagnostic.Label.create
              ~range: (Report.span_to_range
                lx.source
                (Span.create
                  ~start_pos: start
                  ~end_pos: lx.pos)
              )
              ~priority: Grace.Diagnostic.Priority.Primary
              (Grace.Diagnostic.Message.of_string "here")
          ])
    | _ ->
      is_done := true
  done;

  if not !is_float then Token.Int (int_of_string (Buffer.contents buf))
  else Token.Float (float_of_string (Buffer.contents buf))

(* parses id *)
let lex_id (lx: lexer) : Token.kind =
  let buf = Buffer.create 16 in
  let is_done = ref false in

  while not !is_done do
    match peek lx with
    | Some c when is_id lx ->
      Buffer.add_char buf c;
      bump lx;
    | Some c when is_number lx ->
      Buffer.add_char buf c;
      bump lx;
    | _ ->
      is_done := true
  done;

  id_to_tk (Buffer.contents buf)

(* retrieves next token *)
let next_token (lx: lexer) : Token.t option =
  (* skipping trivia *)
  skip_trivia lx;
  let span_start = lx.pos in

  (* parsing token kind *)
  let kind = match peek lx, next lx with
  | Some '>', Some '=' -> bump2 lx; Some Token.Ge
  | Some '<', Some '=' -> bump2 lx; Some Token.Le
  | Some '=', Some '=' -> bump2 lx; Some Token.Eq2
  | Some '!', Some '=' -> bump2 lx; Some Token.BangEq
  | Some '|', Some '|' -> bump2 lx; Some Token.Bar2
  | Some '>', _ -> bump lx; Some Token.Gt
  | Some '<', _ -> bump lx; Some Token.Lt
  | Some '=', _ -> bump lx; Some Token.Eq
  | Some '!', _ -> bump lx; Some Token.Bang
  | Some '|', _ -> bump lx; Some Token.Bar
  | Some '(', _ -> bump lx; Some Token.Lparen
  | Some ')', _ -> bump lx; Some Token.Rparen
  | Some '{', _ -> bump lx; Some Token.Lbrace
  | Some '}', _ -> bump lx; Some Token.Rbrace
  | Some '+', _ -> bump lx; Some Token.Plus
  | Some '-', _ -> bump lx; Some Token.Minus
  | Some '*', _ -> bump lx; Some Token.Star
  | Some '/', _ -> bump lx; Some Token.Slash
  | Some '%', _ -> bump lx; Some Token.Percent
  | Some ',', _ -> bump lx; Some Token.Comma
  | Some '.', _ -> bump lx; Some Token.Dot
  | Some '_', _ -> bump lx; Some Token.Wildcard
  | Some '"', _ -> Some (lex_string lx)
  | _ when is_number lx -> Some (lex_number lx)
  | _ when is_id lx -> Some(lex_id lx)
  | _, _ -> None in
  let span_end = lx.pos in

  (* forming token *)
  match kind with
  | Some kind -> Some { kind; span = {start_pos = span_start; end_pos = span_end} }
  | None -> None

(* performs lexing of all the buffer *)
let rec lex_all lx =
  match next_token lx with
  | Some tk -> tk :: lex_all lx
  | None -> []
