let for_all p l =
  List.fold_left
    (fun b element -> b && p element) 
    true 
    l ;;

let exists p l =
  List.fold_left
    (fun b element -> b || p element) 
    false
    l ;;

let sorted cmp l =
  match l with
  | [] -> true
  | x::xs -> 
      let r =	(List.fold_left 
                 (fun accu  e -> 
                    match accu with
                    | Some pe when (cmp pe e) <= 0 -> Some(e)
                    | _ -> None) 
                 (Some (x)) (xs))
      in 	
    		if r = None then false else true;;

