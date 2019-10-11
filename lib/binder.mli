open! Core_kernel

module Id : sig
  include Comparable.S

  val to_string : t -> string
end

module Node : sig
  type t [@@deriving sexp_of]

  val name : t -> string
  val kind : t -> Lexer.Line.Status.t
  val description : t -> string list
  val named_dependents : t -> string list
end

module Path : sig
  module Element : sig
    type t =
      | Header of string
      | Todo of string
    [@@deriving compare, equal, sexp]

    val to_string : t -> string
  end

  type t = Element.t list [@@deriving sexp_of]

  include Comparable.S with type t := t
end

module Section : sig
  type t [@@deriving sexp_of]

  val path : t -> Path.t
  val description : t -> string list
end

module Graph : sig
  type t [@@deriving sexp_of]

  val top_level_description : t -> string sexp_list
  val path_to_section : t -> Section.t sexp_list Path.Map.t
  val dependencies : t -> Id.t sexp_list Id.Map.t
  val name_to_ids : t -> Id.t sexp_list Core_kernel.String.Map.t
  val path_to_ids : t -> Id.t sexp_list Path.Map.t
  val nodes : t -> Node.t Id.Map.t
end

val bind : Parser.t -> Graph.t
