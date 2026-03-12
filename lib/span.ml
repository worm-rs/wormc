(* represents location of token or tree node *)
type t = {
  start_pos : int;
  end_pos   : int;
}

(* creates new span *)
let create ~(start_pos: int) ~(end_pos: int) : t =
  { start_pos; end_pos }

(* converts span to string *)
let string_of_span (span: t) =
  string_of_int span.start_pos ^ ".." ^ string_of_int span.end_pos
