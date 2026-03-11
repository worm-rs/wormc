open Worm

let () =
  let lx = Lex.create ">= <+ !+ != 3.14 2 fn if test" in
  Token.print_tks (Lex.lex_all lx)
