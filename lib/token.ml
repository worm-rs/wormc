(* represents token kind *)
type kind =
  | Int of int
  | Float of float
  | Ident of string
  | String of string
  | Plus | Minus | Star | Slash | Percent
  | Eq | Eq2 | BangEq | Gt | Lt | Ge | Le
  | Bang | Amp | Amp2 | Bar | Bar2
  | Type | Let | Fn | Extern | Struct | Enum
  | If | Else | Use | For | As | True | False
  | Lbrace | Rbrace | Lparen | Rparen
  | Comma | Dot | Wildcard

(* represents token *)
type t = {
  kind: kind;
  span: Common.span
}

(* converts token kind to string *)
let string_of_kind (kind: kind) = match kind with
  | Int n -> "Int(" ^ string_of_int n ^ ")"
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | Ident s -> "Ident(" ^ s ^ ")"
  | String s -> "String(" ^ s ^ ")"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Star -> "Star"
  | Slash -> "Slash"
  | Percent -> "Percent"
  | Eq -> "Eq"
  | Eq2 -> "Eq2"
  | BangEq -> "BangEq"
  | Gt -> "Gt"
  | Lt -> "Lt"
  | Ge -> "Ge"
  | Le -> "Le"
  | Bang -> "Bang"
  | Amp -> "Amp"
  | Amp2 -> "Amp2"
  | Bar -> "Bar"
  | Bar2 -> "Bar2"
  | Type -> "Type"
  | Let -> "Let"
  | Fn -> "Fn"
  | Extern -> "Extern"
  | Struct -> "Struct"
  | Enum -> "Enum"
  | If -> "If"
  | Else -> "Else"
  | Use -> "Use"
  | For -> "For"
  | As -> "As"
  | Lbrace -> "Lbrace"
  | Rbrace -> "Rbrace"
  | Lparen -> "Lparen"
  | Rparen -> "Rparen"
  | Comma -> "Comma"
  | Dot -> "Dot"
  | Wildcard -> "Wildcard"
  | True -> "True"
  | False -> "False"

(* converts token to string *)
let string_of_tok (tk : t) = "(" ^ string_of_kind tk.kind ^ ", " ^ Common.string_of_span tk.span ^ ")"

(* prints token list *)
let print_tks tokens =
  print_string "tokens (";
  print_int (List.length tokens);
  print_endline "): ";

  List.iter (fun tk ->
    print_endline (string_of_tok tk)
  ) tokens
