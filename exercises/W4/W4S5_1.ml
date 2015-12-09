let filter p l =
  List.fold_left
  	(fun accu element -> if p element then element::accu else accu) 
  	[] 
  	l ;;

let partition p l =
  List.fold_right
  	(fun element (lpos,lneg) -> if p element then (element::lpos, lneg) else (lpos, element::lneg)) 
  	l
  	([],[]) 
  	 ;;

let rec sort = function
    | [] -> []
    | x::xs -> let (smaller, larger) = partition (fun y -> y < x) xs
               in sort smaller @ (x::sort larger)
  ;;

