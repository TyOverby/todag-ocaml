open! Core_kernel

let rec should_fit_in_section ~section_path ~node_path =
  match section_path, node_path with
  | [], Binder.Path.Element.Todo _ :: _ -> true
  | sh :: sr, nh :: nr ->
    if Binder.Path.Element.equal sh nh
    then should_fit_in_section ~section_path:sr ~node_path:nr
    else false
  | _ -> false
;;

(* [a, b, c] [a, f] -> [b, c] *)
let rec not_present_in_b ~a ~b =
  match a, b with
  | _, [] -> a
  | [], _ -> []
  | ah :: ar, bh :: br ->
    if Binder.Path.Element.equal ah bh then not_present_in_b ~a:ar ~b:br else a
;;

let rec unique_b ~a ~b =
  match a, b with
  | _, [] -> []
  | [], b -> b
  | ah :: ar, bh :: br ->
    if Binder.Path.Element.equal ah bh then unique_b ~a:ar ~b:br else b
;;

let detail_color (status : Lexer.Line.Status.t option) v =
  match status, v with
  | None, `Contents -> "#333333"
  | Some Empty, `Contents -> "#000000"
  | Some Done, `Contents -> "#A0A0A0"
  | Some Uncertain, `Contents -> "#0A05AA"
  | Some Blocked, `Contents -> "#AA050A"
  | None, `Title -> "#000000"
  | Some Empty, `Title -> "#000000"
  | Some Done, `Title -> "#686868"
  | Some Uncertain, `Title -> "#0A05CC"
  | Some Blocked, `Title -> "#CC050A"
;;

let create_node_contents ~title_size ~title ~lines ~kind =
  let lines_concat =
    lines
    |> List.filter ~f:(fun s -> String.length s <> 0)
    |> List.bind ~f:(Softbreak.softbreak ~on:50)
    |> List.map ~f:(fun text ->
           sprintf
             {|<tr><td align="left"><font color="%s">%s</font></td></tr>|}
             (detail_color kind `Contents)
             text)
    |> String.concat ~sep:" "
  in
  sprintf
    {|
   <table border="0" cellborder="0" cellspacing="2">
       <tr>
         <td align="left"><b><font color="%s" point-size="%d">%s</font></b></td>
       </tr>
       %s
   </table>
  |}
    (detail_color kind `Title)
    title_size
    title
    lines_concat
;;

let emit graph =
  let id = ref 0 in
  let sections =
    graph
    |> Binder.Graph.path_to_section
    |> Map.to_alist
    |> List.map ~f:(fun (k, v) -> k, Some v)
  in
  let sections = ([], None) :: sections in
  let nodes = graph |> Binder.Graph.path_to_ids |> Map.to_alist in
  let emit_structure buffer =
    let emit_start_subgraph ~sections path_segment =
      let descriptions_of_all_sections =
        let open List.Let_syntax in
        let%bind sections = Option.to_list sections in
        let%bind section = sections in
        let%bind descriptions = Binder.Section.description section in
        return descriptions
      in
      Printf.bprintf buffer "subgraph cluster_%d {\n" !id;
      Int.incr id;
      Printf.bprintf
        buffer
        "fontname=\"Helvetica Neue\" fontsize=\"12\" label=<%s>\n"
        (create_node_contents
           ~title_size:20
           ~title:(Binder.Path.Element.to_string path_segment)
           ~lines:descriptions_of_all_sections
           ~kind:None)
    in
    let last =
      List.fold sections ~init:None ~f:(fun acc (path, sections) ->
          (match acc, path with
          | None, path -> List.iter path ~f:(emit_start_subgraph ~sections)
          | Some old_path, new_path ->
            not_present_in_b ~a:old_path ~b:new_path
            |> List.iter ~f:(fun _ -> Printf.bprintf buffer "} \n");
            unique_b ~a:old_path ~b:new_path
            |> List.iter ~f:(emit_start_subgraph ~sections));
          nodes
          |> List.filter ~f:(fun (node_path, _) ->
                 should_fit_in_section ~section_path:path ~node_path)
          |> List.bind ~f:(fun (_, nodes) -> nodes)
          |> List.map ~f:(fun node_id ->
                 node_id, Map.find_exn (Binder.Graph.nodes graph) node_id)
          |> List.iter ~f:(fun (node_id, node) ->
                 let text =
                   create_node_contents
                     ~title_size:15
                     ~title:(Binder.Node.name node)
                     ~lines:(Binder.Node.description node)
                     ~kind:(Some (Binder.Node.kind node))
                 in
                 bprintf
                   buffer
                   "node_%s [ fontname=\"Helvetica Neue\" fontsize=\"12\" color=\"%s\" \
                    label=<%s> ]\n"
                   (Binder.Id.to_string node_id)
                   (detail_color (Some (Binder.Node.kind node)) `Title)
                   text);
          Some path)
    in
    Option.iter last ~f:(List.iter ~f:(fun _ -> Printf.bprintf buffer "} \n"))
  in
  let emit_connections buffer =
    graph
    |> Binder.Graph.dependencies
    |> Map.to_alist
    |> List.iter ~f:(fun (to_, from) ->
           List.iter from ~f:(fun from ->
               Printf.bprintf
                 buffer
                 "node_%s -> node_%s\n"
                 (Binder.Id.to_string from)
                 (Binder.Id.to_string to_)))
  in
  let buffer = Buffer.create 1024 in
  Printf.bprintf buffer "digraph G {\n";
  Printf.bprintf buffer "node [shape=record];\n";
  emit_structure buffer;
  emit_connections buffer;
  Printf.bprintf buffer "}\n";
  Buffer.contents buffer
;;
