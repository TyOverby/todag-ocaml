open! Core_kernel

module Line = struct
  module Status = struct
    type t =
      | Empty
      | Done
      | Uncertain
      | Blocked
    [@@deriving sexp]
  end

  module Kind = struct
    type t =
      | Blank
      | Todo_item of Status.t
      | Header of int
      | Depends_on
      | List_item
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

let depends_on_string = "Depends On:"

let classify i line =
  let get_opt s i = if String.length s <= i then None else Some s.[i] in
  let hd s = get_opt s 0 in
  let indent = line |> String.take_while ~f:(( = ) ' ') |> String.length in
  let without_indent = String.drop_prefix line indent in
  let kind, text =
    match hd without_indent with
    | None -> Line.Kind.Blank, ""
    | Some _
      when String.(
             equal
               depends_on_string
               (prefix without_indent (String.length depends_on_string))) ->
      Line.Kind.Depends_on, without_indent
    | Some '-' ->
      Line.Kind.List_item, without_indent |> Fn.flip String.drop_prefix 1 |> String.strip
    | Some '#' ->
      if indent <> 0
      then
        ( Line.Kind.Error
            { message = "headers must not be indented"; contents = without_indent }
        , "" )
      else (
        let hash_count =
          without_indent |> String.take_while ~f:(( = ) '#') |> String.length
        in
        let remaining = String.drop_prefix without_indent hash_count |> String.strip in
        if String.length remaining = 0
        then
          ( Line.Kind.Error
              { message = "empty title for header"; contents = without_indent }
          , "" )
        else Line.Kind.Header hash_count, remaining)
    | Some '[' ->
      let status = get_opt without_indent 1 in
      let closing = get_opt without_indent 2 in
      let rest = String.drop_prefix without_indent 3 |> String.strip in
      (match status, closing with
      | None, _ | _, None ->
        ( Line.Kind.Error
            { message = "incorrectly formatted todo item"; contents = without_indent }
        , "" )
      | Some ']', _ ->
        ( Line.Kind.Error
            { message = "missing space inbetween brackets"; contents = without_indent }
        , "" )
      | Some (' ' | '_'), Some ']' -> Line.Kind.Todo_item Line.Status.Empty, rest
      | Some ('x' | 'X'), Some ']' -> Line.Kind.Todo_item Line.Status.Done, rest
      | Some '?', Some ']' -> Line.Kind.Todo_item Line.Status.Uncertain, rest
      | Some '!', Some ']' -> Line.Kind.Todo_item Line.Status.Blocked, rest
      | Some status, Some ']' ->
        ( Line.Kind.Error
            { message = sprintf "unknown todo status '%c'" status
            ; contents = without_indent
            }
        , "" )
      | Some _, Some _ ->
        ( Line.Kind.Error
            { message = "missing close bracket in todo item"; contents = without_indent }
        , "" ))
    | _ -> Line.Kind.Body, without_indent
  in
  { Line.indent; line = i; kind; text }
;;

let lex in_string = in_string |> String.split_lines |> List.mapi ~f:classify
