type token = 
  | Token of char
  | Operator of char
  | EOF

type expr = 
  | Op of char
  | Xpr of (char * expr list)


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
  
  tokens

let getBindingPow token = 
  match token with
  | '-' | '+' -> 1.0
  | '*' | '/' -> 2.0
  | _ -> 0.0


let rec parse_expresion tokens min_bp = 

  let lhs, tokens = 
    match tokens with
    | Token c :: rest -> (Op c, rest)
    | _ -> failwith "error unexpected token" 
  in 

  let rec loop lhs tokens = 

    match tokens with
    | Operator op :: rest -> 
        let bp = getBindingPow op in

        if bp < min_bp then 
          (lhs, tokens)

        else
          
          let rhs, tokens' = parse_expresion rest bp in 
          let new_lhs = Xpr (op, [lhs; rhs])  in 

          loop new_lhs tokens'
    | _ -> (lhs, tokens)
  in
  loop lhs tokens

let rec print_expr ?(indent=0) e =
  let pad = String.make indent ' ' in
  match e with
  | Op c ->
      Printf.printf "%sOp(%c)\n" pad c
  | Xpr (op, sub_exprs) ->
      Printf.printf "%sXpr(%c)\n" pad op;
      List.iter (fun sub ->
        print_expr ~indent:(indent + 4) sub
      ) sub_exprs

let rec eval_expr expr =
  match expr with
  | Op c -> 
      int_of_char c - int_of_char '0'
  | Xpr (op, exprs) -> 
      match (op, exprs) with
      | ('+', [lhs; rhs]) -> eval_expr lhs + eval_expr rhs
      | ('-', [lhs; rhs]) -> eval_expr lhs - eval_expr rhs
      | ('*', [lhs; rhs]) -> eval_expr lhs * eval_expr rhs
      | ('/', [lhs; rhs]) -> eval_expr lhs / eval_expr rhs
      | _ -> failwith "Invalid expression"



