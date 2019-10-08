module Line : sig
  module Status : sig
    type t =
      | Empty
      | Done
      | Uncertain
      | Blocked
    [@@deriving sexp]
  end

  module Kind : sig
    type t =
      | Blank
      | Todo_item of Status.t
      | Header of int
      | Body
      | Error of { message : string; contents : string }
    [@@deriving sexp]
  end

  type t =
    { indent : int
    ; line : int
    ; kind : Kind.t
    ; text : string
    }
  [@@deriving sexp]
end

val lex : string -> Line.t list
