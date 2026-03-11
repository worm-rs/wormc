(* represents token of the source code *)
type t =
  | Int of int
  | Float of float
  | Ident of string
  | Plus | Minus | Star | Slash | Percent
  | Eq | Eq2 | BangEq | Gt | Lt | Ge | Le
  | Bang | Amp | Amp2 | Bar | Bar2
  | Type | Let | Fn | Extern | Struct | Enum
  | If | Else | Use | For | As
  | Lbrace | Rbrace | Lparen | Rparen
