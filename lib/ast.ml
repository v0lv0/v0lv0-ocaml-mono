type binOp = Add | Sub | Mul | Div
type value = VInt of int | VFloat of float

type expr =
  | ENum of value
  | EBool of bool
  | EVar of string
  | EBin of binOp * expr * expr
  | ELet of string * expr * expr
  | EIfElse of expr * expr * expr
  | EFun of string * expr
  | EApp of expr * expr
