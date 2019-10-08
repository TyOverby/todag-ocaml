open! Core_kernel

let rec starts_with ~section_path ~node_path =
  match section_path, node_path with
  | [], [ _ ] -> true
  | sh :: sr, nh :: nr ->
    if String.equal sh nh then starts_with ~section_path:sr ~node_path:nr else false
  | _ -> false
;;

(* [a, b, c] [a, f] -> [b, c] *)
let rec not_present_in_b ~a ~b =
  match a, b with
  | _, [] -> a
  | [], _ -> []
  | ah :: ar, bh :: br -> if String.equal ah bh then not_present_in_b ~a:ar ~b:br else a
;;

let rec unique_b ~a ~b =
  match a, b with
  | _, [] -> []
  | [], b -> b
  | ah :: ar, bh :: br -> if String.equal ah bh then unique_b ~a:ar ~b:br else b
;;

let emit graph =
  let sections = graph |> Binder.Graph.path_to_section |> Map.keys in
  let nodes = graph |> Binder.Graph.path_to_ids |> Map.to_alist in
  let emit_structure buffer =
    let last =
      List.fold sections ~init:None ~f:(fun acc key ->
          (match acc, key with
          | None, path ->
            List.iter path ~f:(fun path_segment ->
                Printf.bprintf buffer "subgraph cluster_%s {\n" path_segment)
          | Some old_path, new_path ->
            not_present_in_b ~a:old_path ~b:new_path
            |> List.rev
            |> List.iter ~f:(fun _ -> Printf.bprintf buffer "} \n");
            unique_b ~a:old_path ~b:new_path
            |> List.iter ~f:(Printf.bprintf buffer "subgraph cluster_%s {\n"));
          nodes
          |> List.filter ~f:(fun (node_path, _) ->
                 starts_with ~section_path:key ~node_path)
          |> List.iter ~f:(fun (_, node) ->
                 bprintf buffer "node n_%s" (Binder.Id.to_string node));
          Some key)
    in
    Option.iter last ~f:(List.iter ~f:(fun _ -> Printf.bprintf buffer "} \n"))
  in
  let buffer = Buffer.create 1024 in
  Printf.bprintf buffer "digraph G {\n";
  emit_structure buffer;
  Printf.bprintf buffer "}\n";
  Buffer.contents buffer
;;
