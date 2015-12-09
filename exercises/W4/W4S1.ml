type int_ff = int -> int;;

let rec compose = function 
	| [] -> fun x -> x + 0
	| f::g -> fun x -> f ((compose g) x);;  	

let rec fixedpoint f start delta =
  "Replace this string with your implementation." ;;

