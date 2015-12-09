let rec last_element = function
  | [] ->  invalid_arg "last_element"
  | [x] -> x
  | x::xs -> last_element xs;;

let rec is_sorted = function
  | [] ->  true
  | [x] -> true
  | x::y::ys -> 
      (x < y) && is_sorted (y::ys);;
