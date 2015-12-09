type queue = int list * int list;;

let is_empty (front, back) =
  let queue = front @ List.rev back in
  match queue with
  	| [] -> true
  	| x :: xs -> false;;

let enqueue x (front, back) = 
	(front, [x] @ back);;
	
let split l =
  let ll = List.length l  in
  let rec split_helper i accu queue =
    match queue with
    | [] -> ([],[])
    | x :: xs -> 
        if i >= ll / 2 || i >= ll / 2 + 1 then (List.rev xs, List.rev (x :: accu))
        else split_helper (i+1) (x :: accu) xs
  in split_helper 1 [] l;;

let dequeue (front, back) =
  match (front, List.rev back) with
  | ([],[]) -> (0, ([],[]))
  | ([], x::xs) -> (x, ([], List.rev xs))
  | (x::xs, _) -> (x, (xs, back));;
