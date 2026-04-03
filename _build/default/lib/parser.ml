open Lexer
open Ast

let rec parser_h (tokens : token list) : ( expr * token list )=

  match tokens with
  | h :: tl ->
    (
      match h with
      | TInt i -> (ENum (VInt i), tl)
      | TFloat i ->( ENum (VFloat i), tl)
    )

    | [] -> failwith "empty expr"

let parser (i : token list) : expr=
  match parser_h i with
  |(a,b) -> a
