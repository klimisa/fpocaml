type operation =
    Op of string * operation * operation
  | Value of int;;

type env = (string * (int -> int -> int)) list;;

let rec lookup_function n = function (l:env) ->
  match l with
  | [] -> invalid_arg "lookup_function"
  | (f,v):: xs ->
      if f = n then v
      else lookup_function n xs;;

let add_function name op env =
  let rec lookup_function (l:env) = 
    match l with
    | [] -> env @ [(name,op)]
    | (f,v):: xs ->
        if f = name then env
        else lookup_function xs
  in lookup_function env;; 

let my_env =
  initial_env @ [("min", function a -> (function b -> if a < b then a else b))];;

let rec compute env op  =
  match op with
  | Value i -> i
  | Op (n, o1, o2) -> 
      let f = lookup_function n env in
      f (compute env o1) (compute env o2);;

let rec compute_eff env = function
  | Value i -> i
  | Op (n, o1, o2) -> 
      (lookup_function n env) (compute_eff env o1) (compute_eff env o2);;

