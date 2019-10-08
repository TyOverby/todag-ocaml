open! Core
open! Todag

let to_dot =
  Command.basic
    ~summary:"convert to graphviz dot file"
    (let open Command.Let_syntax in
    let%map_open file = anon ("FILE" %: string) in
    fun () ->
      In_channel.read_all file
      |> Todag.Lexer.lex
      |> Todag.Parser.parse
      |> Todag.Binder.bind
      |> Todag.Dot.emit
      |> print_endline)
;;

;;
Command.(run (group ~summary:"misc processing of todag files" [ "dot", to_dot ]))
