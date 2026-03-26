type token =
  | TInt    of int
  | TFloat  of float
  | TBool   of bool
  | TIdent  of string
  | TLet
  | TIn
  | TIf
  | TThen
  | TElse
  | TEquals
  | TPlus
  | TMinus
  | TTimes
  | TDiv
  | TLParen
  | TRParen
  | TEOF

let is_digit c = c >= '0' && c <= '9'

let rec consume_digit (s: char list) (acc: int) : (int * char list) =
  match s with
  | h :: tl when is_digit h -> consume_digit tl (acc*10+(Char.code h - Char.code '0'))
  | _ -> (acc, s)


let rec tokenize_h (input:char list) : token list =
  match input with
  | [] -> [TEOF]
  | c :: rest ->
    (match c with
    | '\n' | '\t' | ' ' -> tokenize_h rest
    | '+' -> TPlus :: tokenize_h rest
    | '-' -> TMinus :: tokenize_h rest
    | '/' -> TDiv :: tokenize_h rest
    | '*' -> TTimes :: tokenize_h rest
    | '0'..'9' -> (match (consume_digit (input) 0) with |(a,b) -> TInt a :: tokenize_h b)
    | _ -> failwith (Printf.sprintf "Unexpected character")
    )

let tokenize input = tokenize_h (List.of_seq (String.to_seq input))
