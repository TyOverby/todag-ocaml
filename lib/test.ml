open! Core_kernel

let run_lex text = text |> Lexer.lex |> [%sexp_of: Lexer.Line.t list] |> print_s

let run_parse text =
  text |> Lexer.lex |> Parser.parse |> [%sexp_of: Parser.Pretty.t] |> print_s
;;

let run_bind text =
  text
  |> Lexer.lex
  |> Parser.parse
  |> Binder.bind
  |> [%sexp_of: Binder.Graph.t]
  |> print_s
;;

let%expect_test "blank lex" =
  run_lex {|
    
    |};
  [%expect
    {|
      (((indent 0) (line 0) (kind Blank) (text ""))
       ((indent 4) (line 1) (kind Blank) (text ""))
       ((indent 4) (line 2) (kind Blank) (text ""))) |}]
;;

let%expect_test "header lex" =
  run_lex {|
# Header 1 
## Header 1 
### Header 1 |};
  [%expect
    {|
      (((indent 0) (line 0) (kind Blank) (text ""))
       ((indent 0) (line 1) (kind (Header 1)) (text "Header 1"))
       ((indent 0) (line 2) (kind (Header 2)) (text "Header 1"))
       ((indent 0) (line 3) (kind (Header 3)) (text "Header 1"))) |}]
;;

let%expect_test "regular text lex" =
  run_lex {|
    here is some text
        and some more text |};
  [%expect
    {|
      (((indent 0) (line 0) (kind Blank) (text ""))
       ((indent 4) (line 1) (kind Body) (text "here is some text"))
       ((indent 8) (line 2) (kind Body) (text "and some more text "))) |}]
;;

let%expect_test "todo items" =
  run_lex
    {|
    [ ] aaaa
    [_] a alt
    [x] bbbb
    [?] cccc
    [!] dddd
    [$] eeee
    [] eeee
    |};
  [%expect
    {|
    (((indent 0) (line 0) (kind Blank) (text ""))
     ((indent 4) (line 1) (kind (Todo_item Empty)) (text aaaa))
     ((indent 4) (line 2) (kind (Todo_item Empty)) (text "a alt"))
     ((indent 4) (line 3) (kind (Todo_item Done)) (text bbbb))
     ((indent 4) (line 4) (kind (Todo_item Uncertain)) (text cccc))
     ((indent 4) (line 5) (kind (Todo_item Blocked)) (text dddd))
     ((indent 4) (line 6)
      (kind (Error (message "unknown todo status '$'") (contents "[$] eeee")))
      (text ""))
     ((indent 4) (line 7)
      (kind
       (Error (message "missing space inbetween brackets") (contents "[] eeee")))
      (text ""))
     ((indent 4) (line 8) (kind Blank) (text ""))) |}]
;;

let%expect_test "nested_headers parse" =
  run_parse {|
# header_1 
## header_2 
# another_1
    |};
  [%expect {|
    ((header_1 (header_2)) (another_1)) |}]
;;

let%expect_test "nested todos parse" =
  run_parse {|
  [ ] a
    [x] b
      [!] c 
    [?] d
  |};
  [%expect {| ((a _ (b x (c !)) (d ?))) |}]
;;

let%expect_test "nested todos and headers " =
  run_parse {|
# header_1 
[ ] a
    [x] b
## header_2
[!] c 
    [?] d
  |};
  [%expect {| ((header_1 (a _ (b x)) (header_2 (c ! (d ?))))) |}]
;;

let%expect_test "description under header" =
  run_parse
    {|
# header_1 
Description_1
continued_2

## header_2
Description_2

continued_2
  |};
  [%expect
    {|
      ((header_1 Description_1 continued_2
        (header_2 Description_2 -linebreak- continued_2))) |}]
;;

let%expect_test "description under todo" =
  run_parse
    {|
[ ] a 
    This is a description of a
    More description here folks
    [ ] b 
        This is a description of b

        More description after linebreak
  |};
  [%expect
    {|
      ((a _ "This is a description of a" "More description here folks"
        (b _ "This is a description of b" -linebreak-
         "More description after linebreak"))) |}]
;;

let%expect_test "multi-line code description" =
  run_parse {|
[ ] a 
    ```ocaml 
    let x = 
        1 + 2
    ```
  |};
  [%expect {|
      ((a _ "```ocaml " "let x = " "1 + 2" ```)) |}]
;;

let%expect_test "nested todos and descriptions" =
  run_parse
    {|
[ ] todo-a 
    description of a 
    [x] sub-todo
    more description of a
  |};
  [%expect
    {|
      ((todo-a _ "description of a " (sub-todo x) "more description of a")) |}]
;;

let%expect_test "bind two unrelated todos" =
  run_bind
    {|
[ ] todo-a
    description-a1
    description-a2
[ ] todo-b
    description-b
    |};
  [%expect
    {|
    ((nodes
      ((1
        ((name todo-a) (kind Empty) (description (description-a1 description-a2))
         (ast ((todo-a _ description-a1 description-a2)))))
       (2
        ((name todo-b) (kind Empty) (description (description-b))
         (ast ((todo-b _ description-b)))))))
     (path_to_ids (((todo-a) (1)) ((todo-b) (2))))
     (name_to_ids ((todo-a (1)) (todo-b (2)))) (dependencies ())
     (path_to_section ()) (top_level_description ())) |}]
;;

let%expect_test "top-level-description" =
  run_bind {|
hello
there
world
    |};
  [%expect
    {|
    ((nodes ()) (path_to_ids ()) (name_to_ids ()) (dependencies ())
     (path_to_section ()) (top_level_description ("" hello there world))) |}]
;;

let%expect_test "nested-nodes" =
  run_bind {|
[ ] aaa
    [x] bbbb
    |};
  [%expect
    {|
    ((nodes
      ((1 ((name aaa) (kind Empty) (description ()) (ast ((aaa _ (bbbb x))))))
       (2 ((name bbbb) (kind Done) (description ()) (ast ((bbbb x)))))))
     (path_to_ids (((aaa) (1)) ((aaa bbbb) (2))))
     (name_to_ids ((aaa (1)) (bbbb (2)))) (dependencies ((1 (2))))
     (path_to_section ()) (top_level_description ())) |}]
;;

let%expect_test "bind nested nodes with header" =
  run_bind {|
# AAA
[ ] aaa
    [x] bbbb
    |};
  [%expect
    {|
    ((nodes
      ((1 ((name aaa) (kind Empty) (description ()) (ast ((aaa _ (bbbb x))))))
       (2 ((name bbbb) (kind Done) (description ()) (ast ((bbbb x)))))))
     (path_to_ids (((AAA aaa) (1)) ((AAA aaa bbbb) (2))))
     (name_to_ids ((aaa (1)) (bbbb (2)))) (dependencies ((1 (2))))
     (path_to_section
      (((AAA)
        (((path (AAA)) (description ())
          (ast
           ((Header (level 1) (title AAA)
             (children
              ((Todo_item (name aaa) (kind Empty)
                (children
                 ((Todo_item (name bbbb) (kind Done) (children ())
                   (dependents ())
                   (token
                    (((indent 4) (line 3) (kind (Todo_item Done)) (text bbbb)))))))
                (dependents ())
                (token
                 (((indent 0) (line 2) (kind (Todo_item Empty)) (text aaa)))))))
             (token (((indent 0) (line 1) (kind (Header 1)) (text AAA))))))))))))
     (top_level_description ())) |}]
;;
