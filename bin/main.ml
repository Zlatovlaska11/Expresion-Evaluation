open Parser

let rec loop () =
  print_string ">> ";

  let input = read_line () in

  if input <> "exit" then (

    let tokens = Tokens.parse_input input in

    let ast, _ = Tokens.parse_expresion tokens 0.0 in 

    let res = Tokens.eval_expr ast in 

    print_int res; print_newline ();
    loop ()
  )

let () = loop ()

