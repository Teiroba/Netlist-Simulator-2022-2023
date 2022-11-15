
(* The type of tokens. *)

type token = 
  | KEY_DATA of (string*string)
  | EOF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val rom: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((string * string) list)
