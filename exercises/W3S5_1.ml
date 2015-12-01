type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;
  
let rec height t = 
  match t with 
  | Empty -> 0
  | Node (l, x, r) -> 
      let hl = (1 + height l) and hr = (1 + height r) in
      if  hl > hr then  hl
      else hr;;

let rec balanced t  = 
  match t with
  | Empty -> true
  | Node (l, x, r) -> 
      (height l = height r) && (balanced l && balanced r);;
  	
