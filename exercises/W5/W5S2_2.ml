type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list;;

let rec print_path path =
   match path with
   | [] -> print_bytes ""
   | [x] -> print_bytes x 
   | x::xs -> 
   		(print_bytes (x^"/")); 
   		print_path xs;;

let rec print_file lvl name =
	if lvl = 0 then 
		print_bytes name
	else
		let _ = print_bytes "|" in
		print_file (lvl - 1) name;;

let rec print_symlink lvl name path =
  	(print_file lvl name);
  	(print_bytes "->"); 
  	(print_path path);
  	 ;;

let rec print_dir lvl name =
  "Replace this string with your implementation." ;;

let print_filesystem root =
  (* This pre-completed structure is only here to help you.
     If it confuses you, don't hesitate to change it. *)
  let rec print_filesystem lvl items =
    "Replace this string with your implementation." in
  print_filesystem 0 root ;;

let rec resolve sym path =
  (* This pre-completed structure is only here to help you.
     If it confuses you, don't hesitate to change it. *)
  let rec resolve acc path =
    "Replace this string with your implementation."  in
  resolve (List.tl (List.rev sym)) path ;;

let rec file_exists root path =
  "Replace this string with your implementation." ;;

(* move print_filesystem here for exercise 8 *)

