type t =
  | Top_level of t list
  | Header of
      { level : int
      ; title : string
      ; children : t list
      ; token : Lexer.Line.t option
      }
  | Todo_item of
      { name : string
      ; kind : Lexer.Line.Status.t
      ; children : t list
      ; dependents : string list
      ; token : Lexer.Line.t option
      }
  | Description of { contents : string; token : Lexer.Line.t option }
  | Linebreak
[@@deriving sexp]

module Pretty : sig
  type nonrec t = t [@@deriving sexp_of]
end

val parse : Lexer.Line.t list -> t
