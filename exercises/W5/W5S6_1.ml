exception Empty ;;

let swap ra rb =
  (ra := !rb; rb := !ra;);;

let update r f =
  let a = !r in
  let a' = f a in
  (r:=a';a);;

let move l1 l2 =
  match !l1 with
  | [] -> raise Empty
  | x::xs -> l2 := x::!l2;l1 := xs;;

let reverse l =
  let rec loop l1 l2 = 
    try
      (move l1 l2;loop l1 l2)
    with
    | Empty -> !l2
  in loop (ref l) (ref []);;

