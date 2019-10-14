open! Core_kernel

type t =
  | Top_level of t list
  | Header of
      { level : int
      ; title : string
      ; children : t list
      ; token : Lexer.Line.t option
      }
  | Dependents of string list
  | Todo_item of
      { name : string
      ; kind : Lexer.Line.Status.t
      ; children : t list
      ; token : Lexer.Line.t option
      }
  | Description of { contents : string; token : Lexer.Line.t option }
  | Linebreak
[@@deriving sexp]

module Pretty = struct
  type nonrec t = t

  let rec sexp_of_t =
    let sexp_of_kind = function
      | Lexer.Line.Status.Blocked -> Sexp.Atom "!"
      | Lexer.Line.Status.Done -> Sexp.Atom "x"
      | Lexer.Line.Status.Empty -> Sexp.Atom "_"
      | Lexer.Line.Status.Uncertain -> Sexp.Atom "?"
    in
    function
    | Top_level l -> l |> List.map ~f:sexp_of_t |> Sexp.List
    | Header { title; children; _ } ->
      Sexp.List (Sexp.Atom title :: List.map children ~f:sexp_of_t)
    | Todo_item { name; kind; children; _ } ->
      Sexp.List (Sexp.Atom name :: sexp_of_kind kind :: List.map children ~f:sexp_of_t)
    | Description { contents; _ } -> Sexp.Atom contents
    | Linebreak -> Sexp.Atom "-linebreak-"
    | Dependents l -> l |> List.map ~f:(fun a -> Sexp.Atom a) |> Sexp.List
  ;;
end

let null_indent = -1
let max_header = Int.max_value

let rec parse_at ~indent_level ~header_level tokens =
  let recurse = parse_at ~indent_level ~header_level in
  match tokens with
  | [] -> [], []
  | { Lexer.Line.kind = Blank; _ } :: rest ->
    let a, b = recurse rest in
    let a =
      match a with
      | Description _ :: _ -> Linebreak :: a
      | _ -> a
    in
    a, b
  | ({ Lexer.Line.kind = Header lvl; text; _ } as token) :: rest ->
    if lvl <= header_level
    then [], tokens
    else (
      let children, rem = parse_at ~indent_level:null_indent ~header_level:lvl rest in
      let continued, rem = recurse rem in
      let token = Some token in
      Header { level = lvl; title = text; children; token } :: continued, rem)
  | ({ Lexer.Line.kind = Todo_item status; indent; text; _ } as token) :: rest ->
    if indent <= indent_level
    then [], tokens
    else (
      let children, rem = parse_at ~indent_level:indent ~header_level:max_header rest in
      let continued, rem = recurse rem in
      ( Todo_item { name = text; kind = status; children; token = Some token }
        :: continued
      , rem ))
  | { Lexer.Line.kind = Error _; _ } :: rest -> parse_at ~indent_level ~header_level rest
  | ({ Lexer.Line.kind = Body; text; indent; _ } as token) :: rest ->
    if indent <= indent_level
    then [], tokens
    else (
      let continued, rem = recurse rest in
      Description { contents = text; token = Some token } :: continued, rem)
  | { Lexer.Line.kind = Depends_on inline_dep; indent; _ } :: rest ->
    let items, rest =
      List.split_while rest ~f:(function
          | { Lexer.Line.kind = List_item; indent = item_indent; _ }
            when indent = item_indent ->
            true
          | _ -> false)
    in
    let items = items |> List.map ~f:(fun a -> a.text) in
    let items = Option.to_list inline_dep @ items |> Dependents in
    let continued, rem = recurse rest in
    items :: continued, rem
  | { Lexer.Line.kind = List_item; _ } :: _ -> failwith "fail"
;;

let parse tokens =
  let children, _rem = parse_at ~indent_level:null_indent ~header_level:0 tokens in
  Top_level children
;;
