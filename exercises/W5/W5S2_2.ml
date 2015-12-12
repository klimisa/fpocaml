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
   		(print_bytes (x^"/"); 
   		print_path xs);;

let rec print_file lvl name =
	if lvl = 0 then 
		print_bytes name
	else
		(print_bytes "|";
		print_file (lvl - 1) name);;

let rec print_symlink lvl name path =
  	(print_file lvl name;
  	 print_bytes "->"; 
  	 print_path path);;

let rec print_dir lvl name =
	if lvl = 0 then
		(print_bytes "/"; 
		print_bytes name)
	else
		(print_bytes "|";
		print_dir (lvl - 1) name);;

let print_filesystem root =
  let rec print_filesystem lvl items =
    match items with
    | [] -> print_bytes ""
    | (name,node)::ns -> 
        match node with 
        | File ->
            begin
              print_file lvl name; 
              print_newline();
              print_filesystem lvl ns
            end
        | Dir filesystem ->
            begin      
              print_dir lvl name; 
              print_newline(); 
              print_filesystem (lvl+1) filesystem;
              print_filesystem lvl ns
            end
        | Symlink path -> 
            begin       
              print_symlink lvl name path; 
              print_newline();
              print_filesystem lvl ns
            end
  in print_filesystem 0 root ;; 

let rec resolve sym path = 
  let rec resolves acc path =
    match path with
    | [] -> List.rev acc
    | x::xs -> 
        if x = ".." then
          if List.length acc >= 1 then
            resolves (List.tl acc) xs
          else
            resolves acc xs
        else 
          resolves ([x]@acc) xs
  in resolves (List.tl (List.rev sym)) path ;;
  
  let rec file_exists root path =
  match path with
  | [] -> false
  | p::ps ->
  	  match root with
	  | [] -> false
	  | (name,node)::ns ->
  	      if name = p then 
            match node with
            | File -> true
            | Dir filesystem -> file_exists filesystem ps
            | Symlink path ->  true
          else file_exists ns path;;

let print_filesystem root =
  let rec print_filesystem lvl items sym =
    match items with
    | [] -> print_bytes ""
    | (name,node)::ns -> 
        match node with
        | File ->
            begin
              print_file lvl name; 
              print_newline();
              print_filesystem lvl ns sym
            end
        | Dir filesystem ->
            begin
              print_dir lvl name; 
              print_newline(); 
              print_filesystem (lvl+1) filesystem (sym@[name]);
              print_filesystem lvl ns sym;
            end
        | Symlink path -> 
        	let valid_path =
          	if file_exists root (resolve (sym@[name]) path) then
          		path
          	else 
          		["INVALID"] in
          	  begin
			          print_symlink lvl name valid_path; 
			          print_newline();
			          print_filesystem lvl ns sym
	            end
  in print_filesystem 0 root [];; 

