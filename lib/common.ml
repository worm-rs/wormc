(* represents location of token or tree node *)
type span = {
  start_pos : int;
  end_pos   : int;
}

(* converts span to string *)
let string_of_span (span: span) =
  string_of_int span.start_pos ^ ".." ^ string_of_int span.end_pos
