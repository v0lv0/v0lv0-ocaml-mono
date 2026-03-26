open Camlang

let () =
  let tokens = Lexer.tokenize "5213 + 3" in
  List.iter (fun tok ->
    match tok with
    | Lexer.TInt n -> Printf.printf "TInt(%d)\n" n
    | Lexer.TPlus  -> print_endline "TPlus"
    | Lexer.TEOF   -> print_endline "TEOF"
    | _            -> print_endline "OTHER"
  ) tokens
