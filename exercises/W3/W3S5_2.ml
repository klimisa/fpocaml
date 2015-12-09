type clist =
  | CSingle of char
  | CApp of clist * clist
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
  match (l1, l2) with
  | (_, CEmpty) -> l1 
  | (CEmpty, _) -> l2 
  | (_, _) -> CApp (l1, l2);;

let hd l = 
  let rec hd_helper = function
    | CEmpty -> None
    | CSingle x -> Some x
    | CApp (x, y) ->
        let hdx = hd_helper x in 
        if hdx = None 
        then hd_helper y
        else hdx
  in hd_helper l;;

hd
  (CApp 
  	(CApp (CSingle (-3), CApp (CEmpty, CEmpty)),
     CApp (CSingle (-4), CSingle (-3))
    )
  )
    
    
(* Computing hd (CApp (CApp (CEmpty, CEmpty), CApp (CSingle 1, CApp (CSingle 4, CEmpty)))) *)
(* Wrong value None  *)

let tl l =
  let rec tl_helper = function
    | CEmpty -> None 
    | CSingle x -> Some CEmpty 
    | CApp (x, y) -> 
	    	if tl_helper x = None then tl_helper y
	    	else if tl_helper x = Some CEmpty then Some y
  	  	else tl_helper x
  in tl_helper l;;

-----------------------------
S1
--------------------------
 (CApp (
 			CApp (CSingle 'p', CSingle 'd'), 
 			CEmpty))
  
-----------------------------
S2
--------------------------  
(CApp(CSingle (-3),  ---> x Some CEmpty
	CApp (CSingle (-3), CEmpty)) ---> y
	
-----------------------------
Some CEmpty
fib x = if x <= 1 then 1 else fib (x - 1) + fib (x - 2);;


fib 3 = if 3 <= 1 then 1 else fib (3 - 1) + fib (3 - 2);;

1.........
fib 1 = 1;;

2...
	fib 2 = if 2 <= 1 then 1 else fib (2 - 1) + fib (2 - 2);;
	
	1.....
	fib 1 = 1;;
	
	2.....
	fib 0 = 1;;
	


--------------------------  	


  
  
  
  
  
  
  
