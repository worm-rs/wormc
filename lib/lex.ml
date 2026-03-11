(* defines lexer structure *)
type lexer = {
  (* source code we lexing *)
  src: string;
  (* position *)
  mutable pos: int;
}

(* defines lexer error *)
type lexer_error =
  | Unterminated_comment of int * int
  | Unexpected_char of char * int * int

(* creates new lexer *)
let create (src : string) : lexer = { src; pos = 0 }

(* peeks current char *)
let peek (lx : lexer) : char option =
  if lx.pos >= String.length lx.src then None
  else Some(lx.src.[lx.pos])

(* peeks next char *)
let next (lx : lexer) : char option =
  if lx.pos + 1 >= String.length lx.src then None
  else Some(lx.src.[lx.pos])

(* bumps char by adding 1 to pos *)
let bump (lx : lexer) =
  lx.pos <- lx.pos + 1

(* bumps chars by adding 2 to pos *)
let bump2 (lx : lexer) =
  lx.pos <- lx.pos + 2

(* checks current char iswhitespace *)
let is_whitespace (lx : lexer) : bool =
  match peek lx with
  | Some '\n' | Some '\t' | Some ' ' -> true
  | _ -> false

(* skips comment *)
let skip_comment (lx : lexer) =
  (* bumping `(` and `*` *)
  bump2 lx;

  let done_ = ref false in
  while not !done_ do
    match peek lx, next lx with
    | Some '*', Some ')' ->
        bump2 lx; (* `*` and `)` *)
        done_ := true
    | None, _ ->
        failwith "Unterminated comment"
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
  | ident -> Token.Ident ident

let next_token (lx: lexer) : Token.t option =
  skip_trivia lx;
  match peek lx, next lx with
  | Some '>', Some '=' -> ( bump2; Some Token.Ge )
  | Some '<', Some '=' -> 
