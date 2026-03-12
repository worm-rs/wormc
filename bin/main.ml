open Worm
open Grace

let () =
  let contents = "10 + 10 * 2" in
  let source: Grace.Source.t = `String { name = Some "example.wm"; content = contents } in
  let lx = Lex.create contents (ref source) in
  let ps = Parse.create lx (ref source) in
  Pretty_ast.pretty_print_expr (Parse.expr ps)
