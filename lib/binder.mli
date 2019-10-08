open! Core_kernel
module Id : Comparable.S

module Node : sig
  type t [@@deriving sexp_of]
end

module Path : sig
  type t [@@deriving sexp_of]

  include Comparable.S with type t := t
end

module Section : sig
  type t [@@deriving sexp_of]

  val path : t -> string list
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
