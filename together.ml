let maxAbsoluteVal lst =
  let rec helper lst current_max =
    match lst with
    | [] -> current_max  
    | x :: xs ->
        let abs_x = abs x in  
        helper xs (max abs_x current_max)  
  in
  helper lst 0  

let getnthdigit num n =
  let rec helper num n length =
    match num with
    | 0 -> -1  
    | _ when length = n -> num mod 10  
    | _ -> helper (num / 10) n (length + 1)  
  in
  let num_length = String.length (string_of_int num) in
  if n > num_length || n <= 0 then -1  
  else helper num n 1  

let psum lst =
  let rec helper lst acc =
    match lst with
    | [] -> []  
    | x :: xs ->
        let current_sum = x + acc in
        current_sum :: helper xs current_sum  
  in
  helper lst 0  
