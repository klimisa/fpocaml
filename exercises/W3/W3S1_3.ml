let find a w =
	let rec aux i =
		if i >= (Array.length a) then None
		else if a.(i) = w then Some i
		else
			aux (i + 1)
	in aux 0;;

let default_int = function
	| None -> 0
	| Some x -> x
  ;;

let merge a b =
  match (a,b) with
  | (None, None) -> None
  | (Some x, None) -> Some x
  | (None, Some x) -> Some x
  | (Some x, Some y) -> Some (x+y)
   ;;
