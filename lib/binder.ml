open! Core_kernel

module Id : sig
  type t [@@deriving compare, hash]

  val next : offset:t option -> t
  val to_string : t -> string

  include Sexpable.S with type t := t
  include Comparable.S with type t := t
  include Hashable.S with type t := t
end = struct
  module T = Int
  include T

  let cur = ref 0

  let next ~offset =
    let r = !cur in
    cur := r + 1;
    match offset with
    | Some t -> r - t
    | None -> r
  ;;

  let to_string = Int.to_string
end

module Node = struct
  module Parser = Parser.Pretty

  type t =
    { name : string
    ; kind : Lexer.Line.Status.t
    ; description : string list
    ; ast : Parser.t option
    }
  [@@deriving sexp_of, fields]
end

module Path = struct
  module Element = struct
    type t =
      | Header of string
      | Todo of string
    [@@deriving sexp, compare, equal]

    let to_string = function
      | Header s -> s
      | Todo s -> s
    ;;
  end

  module T = struct
    type t = Element.t list [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

module Section = struct
  type t =
    { path : Path.t
    ; description : string list
    ; ast : Parser.t option
    }
  [@@deriving sexp, fields]
end

module Graph = struct
  type t =
    { nodes : Node.t Id.Map.t
    ; path_to_ids : Id.t list Path.Map.t
    ; name_to_ids : Id.t list String.Map.t
    ; dependencies : Id.t list Id.Map.t
    ; path_to_section : Section.t list Path.Map.t
    ; top_level_description : string list
    }
  [@@deriving fields, sexp_of]

  let empty =
    { nodes = Id.Map.empty
    ; path_to_ids = Path.Map.empty
    ; name_to_ids = String.Map.empty
    ; dependencies = Id.Map.empty
    ; path_to_section = Path.Map.empty
    ; top_level_description = []
    }
  ;;
end

let add_and_append map ~key ~data =
  Map.update map key ~f:(function
      | Some existing -> data :: existing
      | None -> [ data ])
;;

let rec bind ~offset ~parent ~acc ~(path : Path.t) (node : Parser.t) =
  match node with
  | Top_level children ->
    let description, acc =
      List.fold children ~init:([], acc) ~f:(fun (description, acc) child ->
          match child with
          | Description { contents; _ } -> contents :: description, acc
          | Linebreak -> "" :: description, acc
          | other -> description, bind ~offset ~parent:None ~acc ~path:[] other)
    in
    { acc with Graph.top_level_description = description }
  | Header { title; children; _ } ->
    let path = List.append path [ Path.Element.Header title ] in
    let description, acc =
      List.fold children ~init:([], acc) ~f:(fun (description, acc) child ->
          match child, description with
          | Description { contents; _ }, [] -> contents :: description, acc
          | Description { contents; _ }, hd::rst -> (hd ^ " " ^ contents) :: rst, acc
          | Linebreak, _ -> "" :: description, acc
          | other, _ -> description, bind ~offset ~parent:None ~acc ~path other)
    in
    let section = { Section.path; description; ast = Some node } in
    { acc with
      Graph.path_to_section =
        add_and_append acc.Graph.path_to_section ~key:path ~data:section
    }
  | Todo_item { name; kind; children; _ } ->
    let id = Id.next ~offset:(Some offset) in
    let nested_path = List.append path [ Path.Element.Todo name ] in
    let description, acc =
      List.fold children ~init:([], acc) ~f:(fun (description, acc) child ->
          match child, description with
          | Description { contents; _ }, [] -> [ contents ], acc
          | Description { contents; _ }, hd :: rst -> (hd ^ " " ^ contents) :: rst, acc
          | Linebreak, _ -> "" :: description, acc
          | other, _ ->
            description, bind ~offset ~parent:(Some id) ~acc ~path:nested_path other)
    in
    let node = { Node.name; kind; description; ast = Some node } in
    { acc with
      Graph.nodes = Map.add_exn acc.nodes ~key:id ~data:node
    ; path_to_ids = add_and_append acc.path_to_ids ~key:nested_path ~data:id
    ; name_to_ids = add_and_append acc.name_to_ids ~key:name ~data:id
    ; dependencies =
        Option.fold parent ~init:acc.dependencies ~f:(fun acc parent ->
            add_and_append acc ~key:parent ~data:id)
    }
  | Description _ | Linebreak -> acc
;;

let bind node =
  let id = Id.next ~offset:None in
  let graph = bind ~offset:id ~parent:None ~acc:Graph.empty ~path:[] node in
  let rev_node (node : Node.t) = { node with description = List.rev node.description } in
  { Graph.nodes = Map.map graph.nodes ~f:rev_node
  ; path_to_ids = Map.map graph.path_to_ids ~f:List.rev
  ; name_to_ids = Map.map graph.name_to_ids ~f:List.rev
  ; dependencies = Map.map graph.dependencies ~f:List.rev
  ; path_to_section = Map.map graph.path_to_section ~f:List.rev
  ; top_level_description = List.rev graph.top_level_description
  }
;;
