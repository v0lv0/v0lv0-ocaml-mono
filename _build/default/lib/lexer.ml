type token =
  | TInt    of int
  | TFloat  of float
  | TBool   of bool
  | TIdent  of string
  | TLet
  | TFun
  | TArrow
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
let is_alpha c = c >= 'a' && c <= 'z'

let rec consume_digit (s: char list) (acc: int) : (int * char list) =
  match s with
  | h :: tl when is_digit h -> consume_digit tl (acc*10+(Char.code h - Char.code '0'))
  | _ -> (acc, s)

let rec consume_alpha_num (s: char list) (acc: char list) : (char list * char list) =
  match s with
  | h :: tl when is_digit h || is_alpha h -> consume_alpha_num tl (h::acc)
  | _ -> (List.rev acc, s)


let rec tokenize_h (input:char list) : token list =
  match input with
  | [] -> [TEOF]
  | c :: rest ->
    (match c with
    | '\n' | '\t' | ' ' -> tokenize_h rest
    | '+' -> TPlus :: tokenize_h rest
    | '-' ->(
      match rest with
      | d:: rest' -> TArrow :: tokenize_h rest'
      | _ -> TMinus :: tokenize_h rest
    )
    | '/' -> TDiv :: tokenize_h rest
    | '*' -> TTimes :: tokenize_h rest
    | '=' -> TEquals :: tokenize_h rest
    | '0'..'9' -> (match (consume_digit (input) 0) with |(a,b) -> TInt a :: tokenize_h b)
    | 'a'..'z' ->
      let (word_cl, rest') = consume_alpha_num input [] in
      let word_s = String.of_seq (List.to_seq word_cl) in
      (match word_s with
        | "fun" -> TFun :: tokenize_h rest'
        | "let" -> TLet :: tokenize_h rest'
        | "in" -> TIn :: tokenize_h rest'
        | "if" -> TIf :: tokenize_h rest'
        | "then" -> TThen :: tokenize_h rest'
        | "else" -> TElse :: tokenize_h rest'
        | _ -> TIdent word_s :: tokenize_h rest'
        )
    | _ -> failwith (Printf.sprintf "Unexpected character")
    )

let tokenize input = tokenize_h (List.of_seq (String.to_seq input))
