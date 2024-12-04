let maxAbsoluteVal lst =
  let rec helper lst current_max =
    match lst with
    | [] -> current_max  
    | x :: xs ->
        let abs_x = abs x in  
        helper xs (max abs_x current_max)  
  in
  helper lst 0  

let getnthdigit number n =
  if number = 0 && n = 1 then 0
  else
    let digits = string_of_int (abs number) in
    if n > 0 && n <= String.length digits then
      int_of_char (digits.[n - 1]) - int_of_char '0'
    else
      failwith "Index out of bounds"


let psum lst =
  let rec helper lst acc =
    match lst with
    | [] -> []  
    | x :: xs ->
        let current_sum = x + acc in
        current_sum :: helper xs current_sum  
  in
  helper lst 0  
