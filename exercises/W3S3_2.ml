type trie = Trie of int option * char_to_children and char_to_children = (char * trie) list;;

let empty =
  Trie (None, []);;

let example =
  Trie (None,
	[('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	 ('t',
	  Trie (None,
		[('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
	 ('A', Trie (Some 15, []))])

let children_from_char m c =
  let rec children_from_char_helper l =
    match l with 
    | [] -> None
    | (c', t)::xs ->
        if c' = c then 
          Some t
        else children_from_char_helper xs
  in children_from_char_helper m;;

let update_children m c t =
	if children_from_char m c = None then m @ [(c , t)]
	else
		let rec update_children_helper accu l =
		  match l with 
		  | [] -> List.rev accu 
		  | (c', t') :: xs ->
		      if c' = c then 
		       	update_children_helper ((c , t ) :: accu) xs
		      else 
		        update_children_helper ((c', t') :: accu) xs
  	in update_children_helper [] m;;	
  	
let lookup trie w =
  let rec lookup_helper t i =
  	let Trie (x,l) = t in 
		  if i >= String.length w then x
		  else
		    let rec loop = function		  
		      | [] -> None 
		      | (c, t)::xs  ->
		          if c = String.get w i then 
		            lookup_helper t (i+1)
		          else loop xs 
		    in loop l
  in lookup_helper trie 0;;

let insert trie w v =
  let rec insert_helper t i =
  	let Trie (x,l) = t in 
		  if i >= String.length w then x
		  else
		    let rec loop = function		  
		      | [] -> Trie (x, l @ (String.get w i, [])) 
		      | (c, t)::xs  ->
		          if c = String.get w i then 
		            insert_helper t (i+1)
		          else loop xs 
		    in loop l
  in insert_helper trie 0;;
