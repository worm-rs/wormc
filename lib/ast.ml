(* represents literal *)
type lit =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool

(* represents binary operation *)
type bin_op =
  | Add
  | Sub
  | Mul
  | Slash
  | Mod
  | And
  | Or
  | Eq
  | Neq
  | Ge
  | Le
  | Gt
  | Lt

(* represents unary operation *)
type un_op =
  | Neg
  | Not

(* represents type hint *)
and hint = {
  kind: hint_kind;
  span: Span.t;
}

(* represents type hint kind *)
and hint_kind =
  | Local of string
  | Module of string * string
  | Fn of hint * hint list
  | Unit

(* represents visibility *)
type visibility =
  | Pub
  | Priv

(* represents structure field *)
type field = {
  name: string;
  hint: hint;
  span: Span.t
}

(* represents enum variant *)
type variant = {
  name: string;
  fields: hint list;
  span: Span.t;
}

(* represents function parameter *)
type param = {
  name: string;
  hint: hint;
  span: Span.t
}

(* represents item *)
and item = {
  kind: item_kind;
  span: Span.t;
}

(* represents item kind *)
and item_kind =
  (* name, generics, fields *)
  | Struct of string * string list * field list
  (* name, generics, variants *)
  | Enum of string * string list * variant list
  (* name, generics, params, body *)
  | Fn of string * string list * param list * stmt list
  (* name, generics, params, body *)
  | ExtFn of string * string list * param list * string

(* represents statement *)
and stmt = {
  kind: stmt_kind;
  span: Span.t;
}

(* represents statement kind *)
and stmt_kind =
  | Let of string * hint option * expr
  | Expr of expr
  | Semi of expr

(* represents pattern *)
and pat = {
  kind: pat_kind;
  span: Span.t;
}

(* represents pattern kind *)
and pat_kind =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Unpack of expr * string list
  | Variant of expr
  | Or of pat * pat

(* represents single match case*)
and case = {
  pats: pat list;
  body: stmt list;
  span: Span.t
}

(* represents expression *)
and expr = {
  kind: expr_kind;
  span: Span.t
}

(* represents expression kind *)
and expr_kind =
  | Lit of lit
  | Bin of bin_op * expr * expr
  | Unary of un_op * expr
  | If of expr * expr * expr option
  | Id of string
  | Field of expr * string
  | Call of expr * expr list
  | Paren of expr
  | Match of expr * case list
  | Assign of expr * expr
  | Block of stmt list
