(* defines lexer structure *)
type lexer = {
  (* source code we lexing *)
  src: string;
  (* position *)
  mutable pos: int;
}

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

let kw_to_tk (kw: string) =
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
  | "use" -> Token.Use
  | "as" -> Token.As
  | ident -> Token.Ident ident
