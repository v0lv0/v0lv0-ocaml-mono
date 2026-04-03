open Camlang

let print_token tok =
  match tok with
  | Lexer.TInt n    -> Printf.printf "TInt(%d)\n" n
  | Lexer.TFloat f  -> Printf.printf "TFloat(%f)\n" f
  | Lexer.TBool b   -> Printf.printf "TBool(%b)\n" b
  | Lexer.TIdent s  -> Printf.printf "TIdent(%s)\n" s
  | Lexer.TLet      -> print_endline "TLet"
  | Lexer.TFun      -> print_endline "TFun"
  | Lexer.TArrow    -> print_endline "TArrow"
  | Lexer.TIn       -> print_endline "TIn"
  | Lexer.TIf       -> print_endline "TIf"
  | Lexer.TThen     -> print_endline "TThen"
  | Lexer.TElse     -> print_endline "TElse"
  | Lexer.TEquals   -> print_endline "TEquals"
  | Lexer.TPlus     -> print_endline "TPlus"
  | Lexer.TMinus    -> print_endline "TMinus"
  | Lexer.TTimes    -> print_endline "TTimes"
  | Lexer.TDiv      -> print_endline "TDiv"
  | Lexer.TLParen   -> print_endline "TLParen"
  | Lexer.TRParen   -> print_endline "TRParen"
  | Lexer.TEOF      -> print_endline "TEOF"

let test input =
  Printf.printf "\n--- \"%s\" ---\n" input;
  let tokens = Lexer.tokenize input in
  List.iter print_token tokens

let () =
  test "let f = fun x -> x + 1";
  test "if true then 1 else 0";
  test "let x = (3 + 4) * 2"
