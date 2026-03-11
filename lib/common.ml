(* represents location of token or tree node *)
type span = {
  start_pos : int;
  end_pos   : int;
}

(* any type with location *)
type 'a located = {
  value : 'a;
  span  : span;
}

(* converts span to string *)
let string_of_span (span: span) =
  string_of_int span.start_pos ^ ".." ^ string_of_int span.end_pos
