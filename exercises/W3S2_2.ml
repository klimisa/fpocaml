let rec mem x l =
  match l with
  | [] -> false
  | x' :: xs ->	(x' = x || mem x xs) ;;

let rec append l1 l2 = 
	match l1 with
  | [] -> l2
  | hd :: tl -> hd :: (append tl l2);;

let rec combine l1 l2 =
	  match (l1, l2) with
	  | ([],[]) -> []
	  | (x::xs,x'::xs') -> (x,x') :: (combine xs xs');;

let rec assoc l k =
  match l with
  | [] -> None
  | (k', x)::xs -> 
			if k' = k then Some x
			else assoc xs k ;;
