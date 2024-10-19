type t =
  | Integer of int
  | Boolean of bool
  | Return of t
  | Error of string
  | Null
[@@deriving show]
