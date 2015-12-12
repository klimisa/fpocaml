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
  let s0 = sample () in let a0 = compare (exec user s0) (exec reference s0) to_string in 
  let s1 = sample () in let a1 = compare (exec user s1) (exec reference s1) to_string in
  let s2 = sample () in let a2 = compare (exec user s2) (exec reference s2) to_string in
  let s3 = sample () in let a3 = compare (exec user s3) (exec reference s3) to_string in
  let s4 = sample () in let a4 = compare (exec user s4) (exec reference s4) to_string in
  let s5 = sample () in let a5 = compare (exec user s5) (exec reference s5) to_string in
  let s6 = sample () in let a6 = compare (exec user s6) (exec reference s6) to_string in
  let s7 = sample () in let a7 = compare (exec user s7) (exec reference s7) to_string in
  let s8 = sample () in let a8 = compare (exec user s8) (exec reference s8) to_string in
  let s9 = sample () in let a9 = compare (exec user s9) (exec reference s9) to_string in
  ((a0::a1::a2::a3::a4::a5::a6::a7::a8::a9::[]) : report)

