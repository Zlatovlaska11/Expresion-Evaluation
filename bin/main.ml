open Parser

let () = 
  (* Tokens.parse_input "1 + 2 - 3" *)

  let tokens = [Tokens.Token '1'; Tokens.Operator '+'; Tokens.Token '2'; Tokens.Operator '*'; Tokens.Token '6'; Tokens.Operator '/'; Tokens.Token '2'; Tokens.EOF] in 

  let ast, _ = Tokens.parse_expresion tokens 0.0 in 

  let res = Tokens.eval_expr ast in 

  (* Tokens.print_expr ast *)

  print_int res



