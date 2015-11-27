type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp;;

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3));;
  
let my_example =
  EAdd  (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3));;

let eval e =
  let rec eval_helper a = 
    match a with
  		| EInt x -> x
		  | EAdd (x, y) -> (eval_helper x) + (eval_helper y)
		  | EMul (x, y) -> (eval_helper x) * (eval_helper y)
	 in eval_helper e;;

let factorize e = 
  match e with
  | EAdd (EMul (x, y), EMul (x', y')) -> 
      if x = x' then 
        EMul (x, EAdd (y, y')) 
      else 
        e
  | e -> e;;

let expand e =
 	match e with
  | EMul (x, EAdd (y, y')) -> 
      EAdd (EMul (x, y), EMul (x, y')) 
  | e -> e;;

let simplify e =
  match e with
  | EMul (_, EInt 0) -> EInt 0 
  | EMul (EInt 0, _) -> EInt 0 
  | EMul (x,EInt 1) -> x 
  | EMul (EInt 1, x) -> x 
  | EAdd (EInt 0, x) -> x   
  | EAdd (x, EInt 0) -> x   
  | e -> e;;

