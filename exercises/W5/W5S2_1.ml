let rec print_int_list l =
	match l with
	| [] -> print_string "\n"
	| x::xs -> 
			(print_string ((string_of_int x)^"\n"));
			print_int_list xs  
;;

let print_every_other k l =
  match l with
  | [] -> print_string "\n"
  | x::xs -> 
      let rec loop i = 
        let ktimes = k*i in
        if k*i < List.length l then
          let _ = print_string ((string_of_int (List.nth l ktimes))^"\n") in
          loop (i+1)
        else 
          print_string "\n"
      in loop 0
;;

let rec print_list print l =
	match l with
	| [] -> print_string "\n"
	| x::xs -> 
			let _ = print x in
			let _ = print_newline() in
			print_list print xs  
;;
