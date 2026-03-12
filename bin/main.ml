open Worm
open Grace

let () =
  let contents = ">= <+ !+ != 3.14 2 fn if test \"hello" in
  let source: Grace.Source.t = `String { name = Some "example.wm"; content = contents } in
  let lx = Lex.create contents (ref source) in
  Token.print_tks (Lex.lex_all lx)
