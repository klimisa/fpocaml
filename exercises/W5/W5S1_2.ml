type report = message list
and message = string * status
and status = Successful | Failed;;

type 'a result = Ok of 'a | Error of exn;;

let exec f x = 
  try
    let r = f x in Ok r
  with 
  | exn -> Error exn;;

let compare user reference to_string =
  match (user, reference) with
  | (Ok a, Ok b) ->
  	 	if a = b then
  	 		("got correct value " ^ to_string a, Successful)
  	 	else
  	 		("got unexpected value " ^ to_string a, Failed)
  | (Ok a, Error b) -> ("got unexpected value " ^ to_string a, Failed)
  | (Error a, Ok b) -> ("got unexpected exception " ^ exn_to_string a, Failed)
  | (Error a, Error b) ->
  	  	 if a = b then
  	 		("got correct exception " ^ exn_to_string a, Successful)
  	 	else
  	 		("got unexpected exception " ^ exn_to_string a, Failed);;
  

let test user reference sample to_string =
  let rec loop i acc =
  		if i >= 10 then List.rev acc
  		else
      let s = sample () in 
      let a = compare (exec user s) (exec reference s) to_string in
      loop (i+1) (a::acc)
  in loop 0 [];; 

