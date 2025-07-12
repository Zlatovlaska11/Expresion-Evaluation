type token = 
  | Token of char
  | Operator of char
  | EOF

let is_whitespace c =
  c == ' '

let is_digit c =
  c >= '0' && c <= '9'

let getTokenType c =
  match c with
  | '0' .. '9' -> Token c
  | 'a' .. 'z' | 'A' .. 'Z' -> Token c
  | '+' | '-' | '*' | '%' | '/' -> Operator c
  | _ -> EOF

let print_token c =
  let desc = match c with
  | Token n -> "Token " ^ String.make 1 n
  | Operator n -> "Op" ^ String.make 1 n
  | EOF -> "EOF" ^ String.make 1 'n' in 

  print_string (desc ^ "\n")

let parse_input data = 
  let tokens = data 
    |> String.to_seq 
    |> List.of_seq
    |> List.filter (fun c -> not (is_whitespace c))
    |> List.map getTokenType in 
  
  tokens |> List.iter print_token
