let rec mem x l =
  match l with
  | [] -> false
  | x' :: xs ->	(x' = x || mem x xs) ;;

(* TODO: faster version. *)
let rec append l1 l2 = l1 @ l2;;

(* TODO: faster version. *)
let rec combine l1 l2 =
	let rec combine_helper l l1 l2 =
	  match (l1, l2) with
	  | ([],[]) -> l
	  | (x::xs,x'::xs') -> combine_helper (l @ [(x,x')]) xs xs' 
	in combine_helper [] l1 l2;;

let rec assoc l k =
  match l with
  | [] -> None
  | x::xs -> 
  	let (k', x') = x in
			if k' = k then Some x'
			else assoc xs k ;;
