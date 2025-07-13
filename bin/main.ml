open Parser

let () = 
  (* Tokens.parse_input "1 + 2 - 3" *)

  let tokens = [Tokens.Token 'a'; Tokens.Operator '*'; Tokens.Token 'b'; Tokens.Operator '+'; Tokens.Token 'c'; Tokens.EOF] in 

  let ast, _ = Tokens.parse_expresion tokens 0.0 in 

  Tokens.print_expr ast



