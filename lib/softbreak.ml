open! Core_kernel

let softbreak ~on:breakpoint line =
  let last_line, lines =
    line
    |> String.split ~on:' '
    |> List.fold ~init:("", []) ~f:(fun (curline, lines) word ->
           if String.length curline + String.length word > breakpoint
           then word, curline :: lines
           else curline ^ " " ^ word, lines)
  in
  last_line :: lines |> List.map ~f:String.strip |> List.rev
;;

let%expect_test _ =
  let out = softbreak ~on:5 "a b c d e f g h i j k l m n o p" in
  print_s [%message (out : string list)];
  [%expect {| (out ("a b c" "d e f" "g h i" "j k l" "m n o" p)) |}]
;;
