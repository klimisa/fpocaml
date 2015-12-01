type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty;;

let example =
  CApp (CApp (CSingle 1, CSingle 2),
        CApp (CSingle 3,CApp (CSingle 4, CEmpty)));;
              
let to_list l =
	let rec to_list_helper accu = function
	| CEmpty -> accu
  | CSingle a -> a::accu
  | CApp (a, b) -> (to_list_helper accu a) @ (to_list_helper accu b)
in to_list_helper [] l;;
  
let rec of_list l =
  match l with
  | [] -> CEmpty 
  | [x] -> CSingle x
  | x::xs -> CApp (CSingle x, of_list xs) ;;
  
let append l1 l2 =
  "Replace this string with your implementation." ;;

let hd l =
  "Replace this string with your implementation." ;;

let tl l =
  "Replace this string with your implementation." ;;
