open Ast

let indent_str = "│   "
let branch_str = "├── "
let last_branch_str = "└── "

let pp_prefix prefix is_last =
  if is_last then prefix ^ last_branch_str
  else prefix ^ branch_str

let child_prefix prefix is_last =
  if is_last then prefix ^ "    "
  else prefix ^ indent_str

let pp_lit = function
  | Int i    -> Printf.sprintf "Int(%d)" i
  | Float f  -> Printf.sprintf "Float(%g)" f
  | String s -> Printf.sprintf "String(%S)" s
  | Bool b   -> Printf.sprintf "Bool(%b)" b

let pp_bin_op = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | And -> "&&" | Or  -> "||"
  | BitAnd -> "&" | BitOr -> "|" | Xor -> "^"
  | Eq -> "==" | Ne -> "!=" | Ge -> ">=" | Le -> "<=" | Gt -> ">" | Lt -> "<"

let pp_un_op = function
  | Neg -> "-" | Not -> "!"

let rec pp_hint_kind = function
  | Local s         -> s
  | Module (m, n)   -> m ^ "." ^ n
  | Sig (ret, args) ->
    let args_s = String.concat ", " (List.map (fun (h: hint) -> pp_hint_kind h.kind) args) in
    Printf.sprintf "fn(%s) -> %s" args_s (pp_hint_kind ret.kind)
  | Unit -> "()"

(* pp_children: writes "prefix+branch+label\n" then calls print_fn with child_prefix.
   print_fn must print ONLY the children of that node, not the node label itself. *)
let rec pp_children buf prefix children =
  let n = List.length children in
  List.iteri (fun i (label, print_fn) ->
    let is_last = i = n - 1 in
    Buffer.add_string buf (pp_prefix prefix is_last);
    Buffer.add_string buf label;
    Buffer.add_char buf '\n';
    print_fn buf (child_prefix prefix is_last)
  ) children

(* expr_label: compact label including inline info *)
and expr_label e = match e.kind with
  | Lit l           -> pp_lit l
  | Bin (op,_,_)    -> Printf.sprintf "Bin(%s)" (pp_bin_op op)
  | Unary (op,_)    -> Printf.sprintf "Unary(%s)" (pp_un_op op)
  | If _            -> "If"
  | Id s            -> Printf.sprintf "Id(%s)" s
  | Field (_, name) -> Printf.sprintf "Field(.%s)" name
  | Call _          -> "Call"
  | Paren _         -> "Paren"
  | Match _         -> "Match"
  | Assign _        -> "Assign"
  | Block _         -> "Block"

(* prints children of an expr (not the label — pp_children handles that) *)
and pp_expr buf prefix e =
  match e.kind with
  | Lit _ | Id _ -> ()

  | Bin (_, l, r) ->
    pp_children buf prefix
      [ expr_label l, (fun b p -> pp_expr b p l)
      ; expr_label r, (fun b p -> pp_expr b p r) ]

  | Unary (_, e) ->
    pp_children buf prefix [ expr_label e, (fun b p -> pp_expr b p e) ]

  | If (cond, then_, else_opt) ->
    let children =
      [ "if "   ^ expr_label cond,  (fun b p -> pp_expr b p cond)
      ; "then " ^ expr_label then_, (fun b p -> pp_expr b p then_) ]
    in
    let children = match else_opt with
      | Some e -> children @ [ "else " ^ expr_label e, (fun b p -> pp_expr b p e) ]
      | None   -> children
    in
    pp_children buf prefix children

  | Field (e, _) ->
    pp_children buf prefix [ expr_label e, (fun b p -> pp_expr b p e) ]

  | Call (callee, args) ->
    pp_children buf prefix
      ( (expr_label callee, (fun b p -> pp_expr b p callee))
        :: List.map (fun a -> expr_label a, (fun b p -> pp_expr b p a)) args )

  | Paren e ->
    pp_children buf prefix [ expr_label e, (fun b p -> pp_expr b p e) ]

  | Match (e, cases) ->
    pp_children buf prefix
      ( (expr_label e, (fun b p -> pp_expr b p e))
        :: List.mapi (fun i c ->
             Printf.sprintf "case[%d]" i, (fun b p -> pp_case b p c)
           ) cases )

  | Assign (lhs, rhs) ->
    pp_children buf prefix
      [ expr_label lhs, (fun b p -> pp_expr b p lhs)
      ; expr_label rhs, (fun b p -> pp_expr b p rhs) ]

  | Block stmts ->
    pp_children buf prefix
      (List.mapi (fun i s ->
         stmt_label s, (fun b p -> pp_stmt_children b p s)
       ) stmts)

and stmt_label s = match s.kind with
  | Let (name, hint_opt, _) ->
    let h = match hint_opt with
      | Some h -> ": " ^ pp_hint_kind h.kind
      | None   -> ""
    in
    Printf.sprintf "Let(%s%s)" name h
  | Expr _ -> "Expr"
  | Semi _ -> "Semi"

and pp_stmt_children buf prefix s =
  match s.kind with
  | Let (_, _, e) ->
    pp_children buf prefix [ expr_label e, (fun b p -> pp_expr b p e) ]
  | Expr e | Semi e ->
    pp_children buf prefix [ expr_label e, (fun b p -> pp_expr b p e) ]

and pat_label (p: pat) = match p.kind with
  | PInt i    -> Printf.sprintf "PInt(%d)" i
  | PFloat f  -> Printf.sprintf "PFloat(%g)" f
  | PString s -> Printf.sprintf "PString(%S)" s
  | PBool b   -> Printf.sprintf "PBool(%b)" b
  | PUnpack (_, fields) ->
    Printf.sprintf "PUnpack(%s)" (String.concat ", " fields)
  | PVariant _ -> "PVariant"
  | POr _      -> "POr"

and pp_pat_children buf prefix (p: pat) =
  match p.kind with
  | PUnpack (e, _) ->
    pp_children buf prefix [ expr_label e, (fun b p -> pp_expr b p e) ]
  | PVariant e ->
    pp_children buf prefix [ expr_label e, (fun b p -> pp_expr b p e) ]
  | POr (l, r) ->
    pp_children buf prefix
      [ pat_label l, (fun b p -> pp_pat_children b p l)
      ; pat_label r, (fun b p -> pp_pat_children b p r) ]
  | _ -> ()

and pp_case buf prefix c =
  pp_children buf prefix
    ( List.map (fun p -> pat_label p, (fun b pr -> pp_pat_children b pr p)) c.pats
    @ List.map (fun s -> stmt_label s, (fun b pr -> pp_stmt_children b pr s)) c.body )

let pp_item buf (item: item) =
  let label = match item.kind with
    | Struct (name, generics, _) ->
      Printf.sprintf "Struct %s%s" name
        (if generics = [] then "" else "<" ^ String.concat ", " generics ^ ">")
    | Enum (name, generics, _) ->
      Printf.sprintf "Enum %s%s" name
        (if generics = [] then "" else "<" ^ String.concat ", " generics ^ ">")
    | Fn (name, generics, _, _) ->
      Printf.sprintf "Fn %s%s" name
        (if generics = [] then "" else "<" ^ String.concat ", " generics ^ ">")
    | ExtFn (name, generics, _, lib) ->
      Printf.sprintf "ExtFn %s%s [%s]" name
        (if generics = [] then "" else "<" ^ String.concat ", " generics ^ ">")
        lib
  in
  Buffer.add_string buf label;
  Buffer.add_char buf '\n';
  match item.kind with
  | Struct (_, _, fields) ->
    pp_children buf "" (List.map (fun (f: field) ->
      Printf.sprintf "field %s: %s" f.name (pp_hint_kind f.hint.kind),
      fun _ _ -> ()
    ) fields)
  | Enum (_, _, variants) ->
    pp_children buf "" (List.map (fun (v: variant) ->
      Printf.sprintf "variant %s(%s)" v.name
        (String.concat ", " (List.map (fun (h: hint) -> pp_hint_kind h.kind) v.fields)),
      fun _ _ -> ()
    ) variants)
  | Fn (_, _, params, body) ->
    pp_children buf ""
      ( List.map (fun p ->
          Printf.sprintf "param %s: %s" p.name (pp_hint_kind p.hint.kind),
          fun _ _ -> ()
        ) params
      @ List.map (fun s -> stmt_label s, (fun b pr -> pp_stmt_children b pr s)) body )
  | ExtFn (_, _, params, _) ->
    pp_children buf ""
      (List.map (fun p ->
        Printf.sprintf "param %s: %s" p.name (pp_hint_kind p.hint.kind),
        fun _ _ -> ()
      ) params)

let pretty_print_item item =
  let buf = Buffer.create 256 in
  pp_item buf item;
  print_string (Buffer.contents buf)

let pretty_print_expr expr =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (expr_label expr);
  Buffer.add_char buf '\n';
  pp_expr buf "" expr;
  print_string (Buffer.contents buf)
